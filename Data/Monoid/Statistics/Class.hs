{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module     : Data.Monoid.Statistics
-- Copyright  : Copyright (c) 2010,2017, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
module Data.Monoid.Statistics.Class
  ( -- * Type class and helpers
    StatMonoid(..)
  , reduceSample
  , reduceSampleVec
    -- * Data types
  , Pair
  ) where

import           Data.Monoid
import qualified Data.Foldable       as F
import qualified Data.Vector.Generic as G
import Data.Data    (Typeable,Data)
import GHC.Generics (Generic)

-- | This type class is used to express parallelizable constant space
--   algorithms for calculation of statistics.
--
--   Statistic is some measure of sample which doesn't depend on order
--   of elements (for example: mean, sum, number of elements,
--   variance, etc). For many statistics it's possible to construct
--   constant space algorithm which is expressed as fold.
--
--   FIXME: add commutative diagram
--
--   For example
--   (we ignoring considerations about numerical stability):
--
--   > data Mean a = Mean Int a
--   >
--   > estimateMean :: Num a => [a] -> Mean a
--   > estimateMean = foldl step mean0
--   >   where
--   >     mean0 = Mean 0 0
--   >     step x (Mean n sum) = Mean (n+1) (sum + x)
--
--   Additionally it's usually possible to implement function for
--   merging two accumulator states together with resulting
--   accumulator corresponding to union of samples which were used to
--   build initial ones.
--
--   > mergeMeans :: Num a => Mean a -> Mean a -> Mean a
--   > mergeMeans (Mean n1 x1) (Mean n2 x2) = Mean (n1+n2) (x1+x2)
--
--   Now we can notice that @mergeMeans@ and @mean0@ forms a monoid.
--   Now we have accumulator value which corresponds to empty sample, function which corresponds to union of two
--
--   Instance must satisfy following laws. If floating point
--   arithmetics is used then equality should be understood as
--   approximate.
--
--   > 1. pappend x (pappend y mempty) == pappend x mempty <> pappend y mempty
--   > 2. x <> y == y <> x
class Monoid m => StatMonoid m a where
  -- | Add one element to monoid accumulator. P stands for point in
  --   analogy for Pointed.
  addValue :: m -> a -> m
  addValue m a = m <> singletonMonoid a
  {-# INLINE addValue #-}
  -- | State of accumulator corresponding to 1-element sample
  singletonMonoid :: a -> m
  singletonMonoid = addValue mempty
  {-# INLINE singletonMonoid #-}
  {-# MINIMAL addValue | singletonMonoid #-}

-- | Calculate statistic over 'Foldable'. It's implemented in terms of
--   foldl'.
reduceSample :: (F.Foldable f, StatMonoid m a) => f a -> m
reduceSample = F.foldl' addValue mempty

-- | Calculate statistic over vector. It's implemented in terms of
--   foldl'.
reduceSampleVec :: (G.Vector v a, StatMonoid m a) => v a -> m
reduceSampleVec = G.foldl' addValue mempty
{-# INLINE reduceSampleVec #-}


instance (Num a, a ~ a') => StatMonoid (Sum a) a' where
  singletonMonoid = Sum

instance (Num a, a ~ a') => StatMonoid (Product a) a' where
  singletonMonoid = Product

----------------------------------------------------------------
-- Generic monoids
----------------------------------------------------------------

-- | Strict pair. It allows to calculate two statistics in parallel
data Pair a b = Pair !a !b
              deriving (Show,Eq,Ord,Typeable,Data,Generic)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair mempty mempty
  mappend (Pair x y) (Pair x' y') =
    Pair (x <> x') (y <> y')
  {-# INLINABLE mempty  #-}
  {-# INLINABLE mappend #-}

instance (StatMonoid a x, StatMonoid b x) => StatMonoid (Pair a b) x where
  addValue (Pair a b) !x = Pair (addValue a x) (addValue b x)
  singletonMonoid x = Pair (singletonMonoid x) (singletonMonoid x)
  {-# INLINE addValue        #-}
  {-# INLINE singletonMonoid #-}
