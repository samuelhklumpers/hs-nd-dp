{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-|
This module defines a class of non-deterministic dynamic programming problems and the means to solve/optimize them.
The framework targets a slightly different set of problems, but some parts take after: https://hackage.haskell.org/package/HyloDP-1.0.0/docs/HyloDP-Base.html, but targets

This module does:
- provide modeling for non-deterministic dynamic programming
    - this allows the "reward" of an action to depend on multiple successor states
- abstract over the search procedure used to determine the optimal action, allowing for:
    - exhaustive searches over finite domains
    - a golden section search for unimodal objectives over "infinite" domains
    - something smarter if you want?
- solve the problem while returning the entire table of subsolutions
    - (the solver needs the entire table anyway).

It does not:
- use algebraic infrastructure to explain dynamic programming is actually described by a hylomorphism (not sure if this one actually still is)
- use MemoTrie (it explodes for some reason, but generally speaking the hashmap isn't the bottleneck anyway)
- use Semirings to describe formulate optimality and the reported statistics at the type level
-}
module DP (
    DPProblem(..),
    enumSearch,
    uniSearch,
    execDP,
    reducedP,
    reducedDP
) where

import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Hashable ( Hashable )
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, newIORef, modifyIORef')

{-|
The type of dynamic programming problems valued in `v`, with states `s` and actions `a`.

The (usually) Monad `m` can be used to compute something if you need, usually it's one of:
- Identity
- IO/ST
- a transformer stack including memoization

(The decomposition into subproblems is encoded in `actValue`).
-}
data DPProblem s v a m =
    DPProblem {
        -- | The search procedure over `a`.
        search :: Search s v a m,
        -- | Returns the objective value of performing an action `a` in a state `s`.
        actValue :: s -> (s -> m v) -> a -> m v
    }

type Search s v a m = s -> (a -> m v) -> m (v, Maybe a)

{-|
Given a function `s -> Either v [a]` indicating whether a state is either:
- terminal with value `v`
- allows for actions `[a]` to be taken
this procedure exhaustively searches the list for the action with the optimal objective.
-}
enumSearch :: (Ord v, Monad m) => (s -> Either v [a]) -> Search s v a m
enumSearch actOrEnd s actValue' = case actOrEnd s of
    Left  v  -> return (v, Nothing)
    Right as -> do
        vs <- mapM (\ a -> (, a) <$> actValue' a) as
        let !(v, a) = maximumBy (compare `on` fst) vs
        return (v, Just a)

{-|
Given a function `f` and bounds `(a, b)`,
find a local maximum of `f` within `(a, b)` using the golden section search.
If `f` is unimodal, this finds the global maximum on `(a, b)`.

Order of convergence is 1 (linear), rate of convergence is $\phi^{ -1}$ (the golden ratio).
-}
goldenSectionSearch :: (Floating x, Ord y, Monad m) => (x -> m y) -> x -> x -> Int -> m (x, y)
goldenSectionSearch f lo hi maxIter = do
        let mid = split lo hi
        vLo <- f lo
        vHi <- f hi
        vMid <- f mid 
        go maxIter lo hi mid vLo vHi vMid
    where
    invphi = (sqrt 5 - 1) / 2
    split x1 x2 = x1 + invphi * (x2 - x1)
    
    go n x1 x2 x3 v1 v2 v3 
      | n > 0 = do
        let x4 = split x1 x2
        v4 <- f x4
        if v4 > v2 then
            go (n - 1) x1 x4 x2 v1 v4 v2
        else
            go (n - 1) x3 x2 x4 v3 v2 v4
      | otherwise = do
        let x = (x1 + x3) / 2
        !xv <- f x 
        return (x, xv)
{-
The invariant maintained by `go _ a b c` is `b = split a c`.
This is evident in the `v4 > v2` case, since `d = split a b`.

In the other, case we need that `b = split c d`.
Abbreviating $p = \phi$, recall that $1/p = p - 1$ and $p^2 = p + 1$.

We have
b = a + (c - a) / p = 2a + pc - pa - c
d = a + (b - a) / p = 2a + pd - pa - d

b = c + (a + (b - a) / p - c) / p
b = c + (a + (b - a) * (p - 1) - c) * (p - 1)
b = c + (2a - b + pb - pa - c) * (p - 1)
b = c + (2pa - pb + p^2b - p^2a - pc) + (-2a + b - pb + pa + c)
0 = 2c + 3pa - 2pb + p^2b - p^2a - pc - 2a
0 = 2c + 3pa - 2p(2a + pc - pa - c) + (p + 1)(2a + pc - pa - c) - p^2a - pc - 2a
0 = 2c + 3pa - 4pa - 2p^2c + 2p^2a + 2pc + 2pa + p^2c - p^2a - pc + 2a + pc - pa - c - p^2a - pc - 2a
0 = c - p^2c + pc
0 = c - (p + 1)c + pc
0 = 0.
-}
    
{-|
Given an injection `mk` of values `x` into actions `a`,
this procedure performs a golden section search over `x` for the objective of `mk x`. 
-}
uniSearch :: (Ord v, Monad m, Floating x) => Int -> (x -> a) -> x -> x -> Search s v a m
uniSearch maxIter mkAct lo hi _ actValue' = do
    (x, y) <- goldenSectionSearch (actValue' . mkAct) lo hi maxIter
    return (y , Just $ mkAct x)

{-|
`solveDP` solves a `DPProblem` in `IO` from a given initial state, returning the optimal action (if it exists) and its objective value.
Needs a reference to a (non-incorrect) table of actions and objective values.
-}
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

{-|
`execDP` solves a `DPProblem` in `IO` from a given initial state, returning the entire table mapping states to their optimal actions and objective values.
Uses an `IORef` to a `HashMap` for memoization.
-}
execDP :: Hashable s => DPProblem s v a IO -> s -> IO (HM.HashMap s (v, Maybe a))
execDP p s = do
    memRef <- newIORef mempty
    _ <- solveDP p memRef s
    readIORef memRef

{-| A "trick" to reduce the output of `execDP` to only the relevant parts.
Instantiate this with the output of an `execDP` call and rerun `execDP` with this problem instead.
-}
reducedP :: (Monad m, Ord v, Hashable s) => HM.HashMap s (v, Maybe a) -> DPProblem s v a m -> DPProblem s v a m
reducedP table p = DPProblem (enumSearch reducedA) reducedV
    where
    reducedA s = let !(v, a) = table HM.! s in maybe (Left v) (Right . (:[])) a
    reducedV = actValue p

{-|
`reducedDP` solves a `DPProblem` in `IO` from a given initial state, returning the table mapping reachable states to their optimal actions and objective values.

A state `s` is reachable if it's either initial, or the objective value of a reachable state `s'` after taking the optimal action refers to `s`.
-}
reducedDP :: (Hashable s, Ord v) => DPProblem s v a IO -> s -> IO (HM.HashMap s (v, Maybe a))
reducedDP p s = do
    !t <- execDP p s
    execDP (reducedP t p) s