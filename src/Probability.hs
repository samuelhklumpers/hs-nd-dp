{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
This module provides some wrappers for computing some statistics for your `DPProblem`.

Make sure your inputs are independent if you like having correct outputs.
-}
module Probability (
    avg,
    HasMean(..),
    HasVariance(..),
    Mixture(..),
    Log1P(..),
    Mean(..),
    MaxMean(..),
    MinMean(..),
    Variance(..),
) where

import Data.Bifunctor (second)
import Data.Function (on)

{-|
A statistic `V :: X -> v` supports mixtures if:
- there is a function $m$
- $f_X(x) = pf_Y(x) + (1-p)f_Z(x)$ implies $V[X] = m(p, V[Y], V[Z])

The method `weightedAvg` generalizes this to the $n$-ary case.
-}
class Mixture a where
    -- | Compute the value of the statistic for a mixture of distributions.
    weightedAvg :: [(Double, a)] -> a

-- | Compute the value of the statistic for a uniform mixture of distributions.
avg :: Mixture a => [a] -> a
avg = weightedAvg . fmap (1,)

{-|
A wrapper to compute the expected value E[X], also known as the `Identity`.
-}
newtype Mean a = Mean { getMean :: a } deriving (Show, Num, Fractional, Floating, Eq, Ord)

{-| The class of statistics containing enough informations to calculate an expected value.

Remark, if `HasMean a r`, `Mixture a`, and `Mixture r`, it does not necessarily hold that `mean . weightedAvg == weightedAvg . fmap (second mean)`.
For example, see `Log1P`.
-}
class HasMean a r | a -> r where
    -- | Extract the expected value. Generally this should satisfy `mean . fromRational = id`.
    mean :: a -> r

instance HasMean (Mean a) a where
  mean = getMean

instance Fractional a => Mixture (Mean a) where
  weightedAvg xs = Mean $ sum [realToFrac wi * xi | (wi, Mean xi) <- xs] / w
    where
    w = realToFrac $ sum $ fmap fst xs


{-|
A wrapper to compute the expected value of the logarithm of X plus one, E[log(1 + X)].
This is equivalent to the logarithm of the expected geometric growth rate.

Note that all this does is map an incoming value `x` to `log (1 + x)` and outgoing values `x` to `exp(x) - 1`.
-}
newtype Log1P a = Log1P { getLog1P :: a } deriving (Show, Functor, Bounded)

instance Applicative Log1P where
  pure = Log1P
  liftA2 f (Log1P x) (Log1P y) = Log1P (f x y)

instance Floating a => Num (Log1P a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = Log1P . log . (1+) . fromInteger
  negate = fmap negate

instance Floating a => Fractional (Log1P a) where
  fromRational = Log1P . log . (1+) . fromRational
  (/) = liftA2 (/)

instance (HasMean a r, Floating r) => HasMean (Log1P a) r where
  mean = exp . subtract 1 . mean . getLog1P

instance Fractional a => Mixture (Log1P a) where
    weightedAvg xs = Log1P $ sum [realToFrac wi * xi | (wi, Log1P xi) <- xs] / w
        where
        w = realToFrac $ sum $ fmap fst xs

{-|
A wrapper to search for the objective value with the maximal expected value.

This ensures that if `mean x < mean y` then `MaxMean x < MaxMean y`.
-}
newtype MaxMean a = MaxMean { getMaxMean :: a } deriving (Show, Num, Fractional)

instance (HasMean a r, Eq r) => Eq (MaxMean a) where
  (==) = (==) `on` mean . getMaxMean
  (/=) = (/=) `on` mean . getMaxMean

instance (HasMean a r, Ord r) => Ord (MaxMean a) where
  compare = compare `on` mean . getMaxMean

instance (HasMean a r) => HasMean (MaxMean a) r where
    mean = mean . getMaxMean

instance Mixture a => Mixture (MaxMean a) where
  weightedAvg = MaxMean . weightedAvg . fmap (second getMaxMean)

{-|
A wrapper to search for the objective value with the minimal expected value.

This ensures that if `mean x >= mean y` then `MinMean x < MinMean y`.
-}
newtype MinMean a = MinMean { getMinMean :: a } deriving (Show, Num, Fractional)

instance (HasMean a r, Eq r) => Eq (MinMean a) where
  (==) = (==) `on` mean . getMinMean
  (/=) = (/=) `on` mean . getMinMean

instance (HasMean a r, Ord r) => Ord (MinMean a) where
  compare = flip compare `on` mean . getMinMean

instance (HasMean a r) => HasMean (MinMean a) r where
    mean = mean . getMinMean

instance Mixture a => Mixture (MinMean a) where
  weightedAvg = MinMean . weightedAvg . fmap (second getMinMean)

{-|
A wrapper to compute the variance and expected value of X.
Requires its first argument to have expected values to work.
-}
data Variance a b = Variance { unVariance :: !a , getVariance :: !b } deriving (Show)

class HasVariance a r | a -> r where
    variance :: a -> r

instance HasMean a r => HasMean (Variance a b) r where
  mean = mean . unVariance

instance HasVariance (Variance a b) b where
  variance = getVariance

instance (HasMean a b, Num a, Num b) => Num (Variance a b) where
    Variance mx vx + Variance my vy = Variance (mx + my) (vx + vy)
    Variance mx vx * Variance my vy = Variance (mx * my) (vx * vy + vx * mean mx ^ (2 :: Int) + vy * mean my ^ (2 :: Int))
    abs = error "cannot take abs of Variance"
    signum = error "cannot take signum of Variance"
    fromInteger x = Variance (fromInteger x) 0
    negate (Variance mx vx) = Variance (negate mx) vx

instance (HasMean a b, Fractional a, Fractional b) => Fractional (Variance a b) where
  fromRational x = Variance (fromRational x) 0
  recip = error "cannot take recip of Variance"

instance (Fractional b, HasMean a b, Mixture a) => Mixture (Variance a b) where
  weightedAvg xs = Variance mx vx
    where
    mx = weightedAvg $ fmap (second unVariance) xs
    vx = - (mean mx ^ (2 :: Int))
         + sum [realToFrac wi * (variance xi + mean xi ^ (2 :: Int)) | (wi, xi) <- xs] / w
    w  = realToFrac $ sum $ fmap fst xs
