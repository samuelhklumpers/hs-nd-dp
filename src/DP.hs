{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}


-- | This module heavily takes after: https://hackage.haskell.org/package/HyloDP-1.0.0/docs/HyloDP-Base.html
module DP ( module DP ) where

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Either (fromLeft)
import Data.IORef (IORef, readIORef, newIORef, modifyIORef')
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Bifunctor (second)

{-|
A dynamic programming problem:
- `s`: type of states
- `v`: type of scores
- `a`: type of actions

Usually `m` is instantiated to a transformer stack including memoization and writing.
-}
data DPProblem s v a m =
    DPProblem {
        -- | Returns whether the state is terminal with some value, or allows for actions to be taken.
        search :: s -> Search v a m,
        -- | Evaluates the score resulting from taking an action in the given state. (This also encodes the decomposition into subproblems).
        actValue :: s -> (s -> m v) -> a -> m v,
        -- | When `m` is a stack including writing, this can be used to log intermediate solutions
        -- | or compute a more useful representation of the optimal solutions on the fly.
        dpTrace :: (v, s, Either (Maybe a) (a, s)) -> m (v, Maybe a)
    }

type Search v a m = (a -> m v) -> m (v, Maybe a)

enumSearch :: (Ord v, Monad m) => (s -> Either v [a]) -> s -> Search v a m
enumSearch actOrEnd s actValue' = case actOrEnd s of
    Left  v  -> return (v, Nothing)
    Right as -> do
        vs <- mapM (\ a -> (, a) <$> actValue' a) as
        let !(v, a) = maximumBy (compare `on` fst) vs
        return (v, Just a)

invphi :: Floating a => a
invphi = (sqrt 5 - 1) / 2

goldenSectionSearch :: (Floating x, Ord y, Monad m) => (x -> m y) -> x -> x -> Int -> m (x, y)
goldenSectionSearch f lo hi maxIter = do
        let mid = split lo hi
        vLo <- f lo
        vHi <- f hi
        vMid <- f mid 
        go maxIter lo hi mid vLo vHi vMid
    where
    split x1 x2 = x1 + invphi * (x2 - x1)
    
    -- the invariant I is that x2 = split x1 x3
    go n x1 x2 x3 v1 v2 v3 
      | n > 0 = do
        let x4 = split x1 x2
        v4 <- f x4
        if v4 > v2 then
            -- evidently, I is preserved
            go (n - 1) x1 x4 x2 v1 v4 v2
        else
            -- note (1)    
            go (n - 1) x3 x2 x4 v3 v2 v4
      | otherwise = do
        let x = (x1 + x3) / 2
        !xv <- f x 
        return (x, xv)

{-
(1)

b = a + (c - a) / p
d = a + (b - a) / p

a,d,b
    d = a + (b - a) / p
    ok.
    
c,b,d
    b = c + (a + (b - a) / p - c) / p
    b = c + (a + (b - a) * (p - 1) - c) * (p - 1)
    b = c + (2a - b + pb - pa - c) * (p - 1)
    b = c + (2pa - pb + p^2b - p^2a - pc) + (-2a + b - pb + pa + c)
    b = c + (2pa - pb + (p+1)b - (p+1)a - pc) + (-2a + b - pb + pa + c)
    b = c + pa + b - a - pc - 2a + b - pb + pa + c
    b = 2c + 2pa + 2b - 3a - pc - pb
    (p - 1)b = 2c + 2pa - 3a - pc
    (p - 1)(a + (c - a) * (p - 1)) = 2c + 2pa - 3a - pc
    (p - 1)(2a - c + pc - pa) = 2c + 2pa - 3a - pc
    2pa - pc + p^2c - p^2a - 2a + c - pc + pa = 2c + 2pa - 3a - pc
    p^2c - p^2a - pc + pa = c - a
    (p^2 - p)(c - a) = c - a
    c - a = c - a
    ok.
-}
    
-- search an unimodal function: https://en.wikipedia.org/wiki/Golden-section_search
uniSearch :: (Ord v, Monad m, Floating x) => Int -> (x -> a) -> x -> x -> s -> Search v a m
uniSearch maxIter mkAct lo hi _ actValue' = do
    (x, y) <- goldenSectionSearch (actValue' . mkAct) lo hi maxIter
    return (y , Just $ mkAct x)

solveDP :: Hashable s => DPProblem s v a IO -> IORef (HM.HashMap s (v, Maybe a)) -> s -> IO (v, Maybe a)
solveDP p memRef s = do
    mem <- readIORef memRef

    case mem HM.!? s of
        Just r -> return r
        Nothing -> do
            -- stops you from accidentally making a huge thunk by using maximumBy on tuples, for example.
            r@(!_, _) <- search p s $ actValue p s (fmap fst . solveDP p memRef)
            modifyIORef' memRef (HM.insert s r)
            return r

execDP :: Hashable s => DPProblem s v a IO -> s -> IO (HM.HashMap s (v, Maybe a))
execDP p s = do
    memRef <- newIORef mempty
    _ <- solveDP p memRef s
    readIORef memRef

-- | A "trick" to reduce the final map to only the relevant parts
reducedP :: (Monad m, Ord v, Hashable s) => HM.HashMap s (v, Maybe a) -> DPProblem s v a m -> DPProblem s v a m
reducedP table p = DPProblem (enumSearch reducedA) reducedV $ \ (s, _, ea) -> return (s, fromLeft Nothing ea)
    where
    -- NOTE: apparently, the real culprit was this let, having it with a ! fixes the leak
    reducedA s = let !(v, a) = table HM.! s in maybe (Left v) (Right . (:[])) a
    reducedV = actValue p

{-

reducedP :: (Monad m, Ord v, Ord s) => M.Map s (v, Maybe a) -> DPProblem s v a m -> DPProblem s v a m
reducedP table p = DPProblem (enumSearch reducedA) reducedV $ \ (s, _, ea) -> return (s, fromLeft Nothing ea)
    where
    reducedA s = let !(v, a) = table M.! s in maybe (Left v) (Right . (:[])) a
    reducedV = actValue p

solveDP :: (Ord v, MonadMemo s (v, Maybe a) m)
      => DPProblem s v a m -> s -> m (v, Maybe a)
solveDP p = memo $ \ s -> do
    case endOrAct p s of
        Left v   -> dpTrace p (v, s, Left Nothing) >> return (v, Nothing)
        Right as -> do
            let subnodes = val' <$> as
            (v', a) <- maximumBy (compare `on` fst) <$> sequence subnodes
            dpTrace p (v', s, Left $ Just a)
            where
            val' a = (, a) <$> actValue p s (\ s' -> solveDP p s' >>= dpTrace' a s') a
            dpTrace' a s' (v, _) = fst <$> dpTrace p (v, s, Right (a, s'))


solveDP :: forall v s a m. (Ord v, MonadMemo s (v, Maybe a) m)
      => DPProblem s v a m -> s -> m (v, Maybe a)
solveDP p = memo $ \ s -> case search p s of
    Left v -> do
        --_ <- dpTrace p (v, s, Left Nothing)
        return (v, Nothing)
    Right srch -> do
        --(v', a') <- 
        runSearch srch actValue'
        --_ <- dpTrace p (v', s, Left a')
        --return (v', a')
        where
        --dpTrace' a s' (v, _) = fst <$> dpTrace p (v, s, Right (a, s'))
        actValue' = actValue p s (fmap fst . solveDP p {->>= dpTrace' a s'-})

execDP :: (Ord v, Ord s) => DPProblem s v a (Memo s (v, Maybe a)) -> s -> M.Map s (v, Maybe a)
execDP p s = snd $ startRunMemo (solveDP p s)
-}
