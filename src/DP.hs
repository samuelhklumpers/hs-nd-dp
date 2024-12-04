{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}


module DP ( module DP ) where

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Control.Monad.Memo (memo, MonadMemo, startRunMemo, Memo)
import Data.Either (fromLeft)
import Control.Monad.State.Strict (put, get, runStateT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Maybe (fromJust, isNothing)
import Control.Monad.Trans (lift)
import Data.IORef (IORef, readIORef, newIORef, modifyIORef')



-- | This module heavily takes after: https://hackage.haskell.org/package/HyloDP-1.0.0/docs/HyloDP-Base.html

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
        --search :: s -> [(a, v)] -> Either v a,
        --search :: s -> ((v, Maybe a, st), (v, a) -> State st (Either (v, Maybe a) a)),
        search :: s -> Either v (Search v a),
        -- | Evaluates the score resulting from taking an action in the given state. (This also encodes the decomposition into subproblems).
        actValue :: s -> (s -> m v) -> a -> m v
        -- | When `m` is a stack including writing, this can be used to log intermediate solutions
        -- | or compute a more useful representation of the optimal solutions on the fly.
        , dpTrace :: (v, s, Either (Maybe a) (a, s)) -> m (v, Maybe a)
    }

--data Search' s v a = forall st. Search' (s -> Either v ([(v, a)] -> Either (v, a) a) )

-- TODO why don't you just use (a -> v) -> (a, v) you moron

data Search v a = forall st. Search {
    initial :: !(a, st),
    --next    :: st -> (v, a) -> Either (v, a) (a, st)
    next    :: (v, a) -> StateT st (Either (v, a)) a
}
{-
runSearch' :: Monad m => Search' s v a -> s -> (a -> m v) -> m (v, Maybe a)
runSearch' (Search' srch) s actValue' = case srch s of 
    Left  v    -> return (v, Nothing)
    Right iter -> go []
        where
        go xs = case iter xs of
            Left (v, a) -> return (v, Just a)
            Right a -> do
                v <- actValue' a
                go ((v, a) : xs)
-}

runSearch :: forall m v a. Monad m => Search v a -> (a -> m v) -> m (v, Maybe a)
runSearch (Search (a0, st0 :: st) n) actValue' = go a0 st0
    where
    go :: a -> st -> m (v, Maybe a)
    go a st = do
        va <- actValue' a
        case flip runStateT st $ n (va, a) of
            Left (vMax, aMax) -> return (vMax, Just aMax)
            Right (aNext, st') -> go aNext st'

enumSearch :: Ord v => v -> (s -> Either v [a]) -> s -> Either v (Search v a)
enumSearch v0 actOrEnd s = case actOrEnd s of
    Left  v -> Left v
    Right (a0:as) -> Right $ Search {
        initial = (a0, (v0, Nothing, as)),
        next = next'
    }
    Right _ -> error "enumSearch': bad"
    where
    next' (v, a) = do
        (vMax, aMax, as') <- get
        let !(vMax', aMax') = if v >= vMax || isNothing aMax then (v, Just a) else (vMax, aMax)
        case as' of
            [] -> lift $ Left (vMax', fromJust aMax')
            (a'':as'') -> do
                put (vMax', aMax', as'')
                return a''

{-
enumSearch' :: (Show v, Show a, Ord v) => v -> (s -> Either v [a]) -> Search' s v a
enumSearch' v0 actOrEnd s = case actOrEnd s of
    Left x -> Left x
    Right as -> Right $ go as
    where
    go [] vs = Left $ maximumBy (compare `on` fst) vs
    go (a:as) _ = Right a
-}

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

solveDP' :: Hashable s => DPProblem s v a IO -> IORef (HM.HashMap s (v, Maybe a)) -> s -> IO (v, Maybe a)
solveDP' p memRef s = do
    mem <- readIORef memRef

    case mem HM.!? s of
        Just r -> return r
        Nothing -> do
            r <- case search p s of
                Left v -> do
                    return (v, Nothing)
                Right srch -> do
                    runSearch srch (actValue p s (fmap fst . solveDP' p memRef))

            modifyIORef' memRef (HM.insert s r)
            return r

execDP' :: Hashable s => DPProblem s v a IO -> s -> IO (HM.HashMap s (v, Maybe a))
execDP' p s = do
    memRef <- newIORef mempty
    _ <- solveDP' p memRef s
    readIORef memRef

{-
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
-}

execDP :: (Ord v, Ord s) => DPProblem s v a (Memo s (v, Maybe a)) -> s -> M.Map s (v, Maybe a)
execDP p s = snd $ startRunMemo (solveDP p s)

-- | A "trick" to reduce the final map to only the relevant parts
reducedP :: (Monad m, Ord v, Ord s) => M.Map s (v, Maybe a) -> v -> DPProblem s v a m -> DPProblem s v a m
reducedP table v0 p = DPProblem (enumSearch v0 reducedA) reducedV $ \ (s, _, ea) -> return (s, fromLeft Nothing ea)
    where
    reducedA s = let !(v, a) = table M.! s in maybe (Left v) (Right . (:[])) a
    reducedV = actValue p

reducedP' :: (Monad m, Ord v, Hashable s) => HM.HashMap s (v, Maybe a) -> v -> DPProblem s v a m -> DPProblem s v a m
reducedP' table v0 p = DPProblem (enumSearch v0 reducedA) reducedV $ \ (s, _, ea) -> return (s, fromLeft Nothing ea)
    where
    -- NOTE: apparently, the real culprit was this let, having it with a ! fixes the leak
    reducedA s = let !(v, a) = table HM.! s in maybe (Left v) (Right . (:[])) a
    reducedV = actValue p