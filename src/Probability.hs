{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wno-type-defaults #-}
{-# LANGUAGE DeriveFunctor #-}

module Probability ( module Probability ) where
import Data.Function (on)
import Data.Bifunctor (second)

-- * This entire module assumes _everything_ is _always_ **independent**.

-- | This is the alternative to using indicator distributions
-- to represent random choice. If you don't like this, enjoy encoding
-- covariance.
class Mixture a where
    weightedAvg :: [(Double, a)] -> a

avg :: Mixture a => [a] -> a
avg = weightedAvg . fmap (1,)

newtype Mean a = Mean { getMean :: a } deriving (Show, Num, Fractional, Floating)

class HasMean a r | a -> r where
    mean :: a -> r

instance HasMean (Mean a) a where
  mean = getMean

instance Fractional a => Mixture (Mean a) where
  weightedAvg xs = Mean $ sum [realToFrac wi * xi | (wi, Mean xi) <- xs] / w
    where
    w = realToFrac $ sum $ fmap fst xs

newtype LogP1 a = LogP1 { getLogP1 :: a } deriving (Show, Functor)

instance Applicative LogP1 where
  pure = LogP1
  liftA2 f (LogP1 x) (LogP1 y) = LogP1 (f x y)

instance Floating a => Num (LogP1 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = LogP1 . log . (1+) . fromInteger
  negate = fmap negate

instance Floating a => Fractional (LogP1 a) where
  fromRational = LogP1 . log . (1+) . fromRational
  (/) = liftA2 (/)

instance (HasMean a r, Floating r) => HasMean (LogP1 a) r where
  mean = exp . subtract 1 . mean . getLogP1

instance Fractional a => Mixture (LogP1 a) where
    weightedAvg xs = LogP1 $ sum [realToFrac wi * xi | (wi, LogP1 xi) <- xs] / w
        where
        w = realToFrac $ sum $ fmap fst xs

newtype MaxMean a = MaxMean { getMaxMean :: a } deriving (Show, Num, Fractional)

instance (HasMean a r, Eq r) => Eq (MaxMean a) where
  (==) = (==) `on` mean . getMaxMean
  (/=) = (/=) `on` mean . getMaxMean

-- TODO do this with Semigroup instead?
instance (HasMean a r, Ord r) => Ord (MaxMean a) where
  compare = compare `on` mean . getMaxMean

instance (HasMean a r) => HasMean (MaxMean a) r where
    mean = mean . getMaxMean

instance Mixture a => Mixture (MaxMean a) where
  weightedAvg = MaxMean . weightedAvg . fmap (second getMaxMean)

newtype MinMean a = MinMean { getMinMean :: a } deriving (Show, Num, Fractional)

instance (HasMean a r, Eq r) => Eq (MinMean a) where
  (==) = (==) `on` mean . getMinMean
  (/=) = (/=) `on` mean . getMinMean

-- TODO do this with Semigroup instead?
instance (HasMean a r, Ord r) => Ord (MinMean a) where
  compare = flip compare `on` mean . getMinMean

instance (HasMean a r) => HasMean (MinMean a) r where
    mean = mean . getMinMean

instance Mixture a => Mixture (MinMean a) where
  weightedAvg = MinMean . weightedAvg . fmap (second getMinMean)

data Variance a b = Variance { unVariance :: a , getVariance :: b } deriving Show

class HasVariance a r | a -> r where
    variance :: a -> r

instance HasMean a r => HasMean (Variance a b) r where
  mean = mean . unVariance

instance HasVariance (Variance a b) b where
  variance = getVariance

instance (HasMean a b, Num a, Num b) => Num (Variance a b) where
    Variance mx vx + Variance my vy = Variance (mx + my) (vx + vy)
    Variance mx vx * Variance my vy = Variance (mx * my) (vx * vy + vx * mean mx ^ 2 + vy * mean my ^ 2)
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
    vx = - (mean mx ^ 2)
         + sum [realToFrac wi * (variance xi + mean xi ^ 2) | (wi, xi) <- xs] / w
    w  = realToFrac $ sum $ fmap fst xs
