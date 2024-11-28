{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DP
    ( module DP ) where

import qualified Data.Map as M
import Data.List ( sortOn, maximumBy )
import Data.Bifunctor (bimap)
import Control.Monad.Memo (memo, MonadMemo, startRunMemo)
import Data.Foldable (fold)
import Data.Function (on)


{-
dpM :: (Ord v, MonadMemo s (v, Maybe a) m) => (s -> Either v [a])
    -> (s -> a -> (s, v -> v)) -> ((v, s, Maybe a) -> m ())
    -> s -> m (v, Maybe a)
dpM g tv tell = memo $ \ s -> do
    y@(v, a) <- case g s of
        Left v   -> return (v, Nothing)
        Right as -> do
            let subnodes = go <$> as
            (v, a) <- head . sortOn fst <$> sequence subnodes
            return (v, Just a)
            where
            go a = let (s', v') = tv s a in
                bimap v' (const a) <$> dpM g tv tell s'
    tell (v, s, a)
    return y
    
dpM' :: (Ord v, Ord s) => (s -> Either v [a]) -> (s -> a -> (s, v -> v)) -> s -> M.Map s (v, Maybe a)
dpM' g tv s = snd $ startRunMemo (dpM g tv (const $ return ()) s)
-}


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
        endOrAct :: s -> Either v [a],
        -- | Evaluates the score resulting from taking an action in the given state. (This also encodes the decomposition into subproblems).
        actValue :: s -> (s -> m v) -> a -> m v,
        -- | When `m` is a stack including writing, this can be used to log intermediate solutions
        -- | or compute a more useful representation of the optimal solutions on the fly.
        optTrace :: (v, s, Maybe a) -> m ()
    }

solveDP :: (Ord v, MonadMemo s (v, Maybe a) m)
      => DPProblem s v a m -> s -> m (v, Maybe a)
solveDP p = memo $ \ s -> do
    case endOrAct p s of
        Left v   -> optTrace p (v, s, Nothing) >> return (v, Nothing)
        Right as -> do
            let subnodes = val' <$> as
            (v', a) <- maximumBy (compare `on` fst) <$> sequence subnodes
            optTrace p (v', s, Just a)
            return (v', Just a)
            where
            val' a = (, a) <$> actValue p s (fmap fst . solveDP p) a
