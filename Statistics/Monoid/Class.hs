{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module     : Data.Monoid.Statistics
-- Copyright  : Copyright (c) 2010,2017, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
module Statistics.Monoid.Class
  ( -- * Type class and helpers
    StatMonoid(..)
  , reduceSample
  , reduceSampleVec
    -- * Ad-hoc type classes for central moments
  , CalcCount(..)
  , CalcMean(..)
  , HasMean(..)
  , CalcVariance(..)
  , HasVariance(..)
  , calcStddev
  , calcStddevML
  , getStddev
  , getStddevML
    -- * Exception handling
  , Partial(..)
  , SampleError(..)
    -- * Data types
  , Pair(..)
  ) where

import           Control.Exception
import           Control.Monad.Catch (MonadThrow(..))
import           Data.Data           (Typeable,Data)
import           Data.Monoid         hiding ((<>))
import           Data.Semigroup      (Semigroup(..))
import           Data.Traversable    (Traversable)
import           Data.Vector.Unboxed          (Unbox)
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import qualified Data.Foldable       as F
import qualified Data.Vector.Generic as G
import           Numeric.Sum
import GHC.Generics (Generic)
-- COMPAT
import qualified Data.Vector.Generic.Mutable -- Needed for GHC7.4

-- | This type class is used to express parallelizable constant space
--   algorithms for calculation of statistics. By definitions
--   /statistic/ is some measure of sample which doesn't depend on
--   order of elements (for example: mean, sum, number of elements,
--   variance, etc).
--
--   For many statistics it's possible to possible to construct
--   constant space algorithm which is expressed as fold. Additionally
--   it's usually possible to write function which combine state of
--   fold accumulator to get statistic for union of two samples.
--
--   Thus for such algorithm we have value which corresponds to empty
--   sample, merge function which which corresponds to merging of two
--   samples, and single step of fold. Last one allows to evaluate
--   statistic given data sample and first two form a monoid and allow
--   parallelization: split data into parts, build estimate for each
--   by folding and then merge them using mappend.
--
--   Instance must satisfy following laws. If floating point
--   arithmetics is used then equality should be understood as
--   approximate.
--
--   > 1. addValue (addValue y mempty) x  == addValue mempty x <> addValue mempty y
--   > 2. x <> y == y <> x
class Monoid m => StatMonoid m a where
  -- | Add one element to monoid accumulator. It's step of fold.
  addValue :: m -> a -> m
  addValue m a = m `mappend` singletonMonoid a
  {-# INLINE addValue #-}
  -- | State of accumulator corresponding to 1-element sample.
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

instance ( StatMonoid m1 a
         , StatMonoid m2 a
         ) => StatMonoid (m1,m2) a where
  addValue (!m1, !m2) a =
    let !m1' = addValue m1 a
        !m2' = addValue m2 a
    in (m1', m2')
  singletonMonoid a = ( singletonMonoid a
                      , singletonMonoid a
                      )

instance ( StatMonoid m1 a
         , StatMonoid m2 a
         , StatMonoid m3 a
         ) => StatMonoid (m1,m2,m3) a where
  addValue (!m1, !m2, !m3) a =
    let !m1' = addValue m1 a
        !m2' = addValue m2 a
        !m3' = addValue m3 a
    in (m1', m2', m3')
  singletonMonoid a = ( singletonMonoid a
                      , singletonMonoid a
                      , singletonMonoid a
                      )

instance ( StatMonoid m1 a
         , StatMonoid m2 a
         , StatMonoid m3 a
         , StatMonoid m4 a
         ) => StatMonoid (m1,m2,m3,m4) a where
  addValue (!m1, !m2, !m3, !m4) a =
    let !m1' = addValue m1 a
        !m2' = addValue m2 a
        !m3' = addValue m3 a
        !m4' = addValue m4 a
    in (m1', m2', m3', m4')
  singletonMonoid a = ( singletonMonoid a
                      , singletonMonoid a
                      , singletonMonoid a
                      , singletonMonoid a
                      )

instance (Num a, a ~ a') => StatMonoid (Sum a) a' where
  singletonMonoid = Sum

instance (Num a, a ~ a') => StatMonoid (Product a) a' where
  singletonMonoid = Product

instance Real a => StatMonoid KahanSum a where
  addValue m x = add m (realToFrac x)
  {-# INLINE addValue #-}

instance Real a => StatMonoid KBNSum a where
  addValue m x = add m (realToFrac x)
  {-# INLINE addValue #-}

instance Real a => StatMonoid KB2Sum a where
  addValue m x = add m (realToFrac x)
  {-# INLINE addValue #-}


----------------------------------------------------------------
-- Ad-hoc type class
----------------------------------------------------------------

-- | Value from which we can efficiently extract number of elements in
--   sample it represents.
class CalcCount a where
  -- | /Assumed O(1)/. Number of elements in sample.
  calcCount :: a -> Int

-- | Value from which we can efficiently calculate mean of sample or
--   distribution.
class CalcMean a where
  -- | /Assumed O(1)/ Returns @Nothing@ if there isn't enough data to
  --   make estimate or distribution doesn't have defined mean.
  calcMean :: MonadThrow m => a -> m Double
  default calcMean :: (MonadThrow m, HasMean a) => a -> m Double
  calcMean = return . getMean

-- | Same as 'CalcMean' but should never fail
class CalcMean m => HasMean m where
  getMean :: m -> Double


-- | Values from which we can efficiently compute estimate of sample
--   variance or distribution variance. It has two methods: one which
--   applies bias correction to estimate and another that returns
--   maximul likelyhood estimate. For distribution they should return
--   same value.
class CalcVariance a where
  -- | /Assumed O(1)/ Calculate unbiased estimate of variance:
  calcVariance   :: MonadThrow m => a -> m Double
  -- | /Assumed O(1)/ Calculate maximum likelihood estimate of variance:
  calcVarianceML :: MonadThrow m => a -> m Double

-- | Same as 'CalcVariance' but never fails
class CalcVariance a => HasVariance a where
  getVariance   :: a -> Double
  getVarianceML :: a -> Double


-- | Calculate sample standard deviation from unbiased estimation of
--   variance.
calcStddev :: (MonadThrow m, CalcVariance a) => a -> m Double
calcStddev = fmap sqrt . calcVariance

-- | Calculate sample standard deviation from maximum likelihood
--   estimation of variance.
calcStddevML :: (MonadThrow m, CalcVariance a) => a -> m Double
calcStddevML = fmap sqrt . calcVarianceML

-- | Same as 'calcStddev' but never fails
getStddev :: HasVariance a => a -> Double
getStddev = sqrt . getVariance

-- | Same as 'calcStddevML' but never fails
getStddevML :: HasVariance a => a -> Double
getStddevML = sqrt . getVarianceML


----------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------

-- | Identity monad which is used to encode partial functions for
--   'MonadThrow' based error handling. Its @MonadThrow@ instance
--   just throws normal exception.
newtype Partial a = Partial a
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

-- | Call function in partial manner. For example
--
-- > partial . mean
--
--   will throw an exception when called with empty vector as input
partial :: Partial a -> a
partial (Partial x) = x

instance Functor Partial where
  fmap f (Partial a) = Partial (f a)

instance Applicative Partial where
  pure = Partial
  Partial f <*> Partial a = Partial (f a)

instance Monad Partial where
  return = Partial
  Partial a >>= f = f a
  (!_) >> f = f

instance MonadThrow Partial where
  throwM = throw

-- | Exception which is thrown when we can't compute some value
data SampleError
  = EmptySample String
  -- ^ @EmptySample function@: We're trying to compute quantity that 
  | InvalidSample String String
  -- ^ @InvalidSample function descripton@ quantity in question could
  --   not be computed for some other reason
  deriving Show

instance Exception SampleError


----------------------------------------------------------------
-- Generic monoids
----------------------------------------------------------------

-- | Strict pair. It allows to calculate two statistics in parallel
data Pair a b = Pair !a !b
              deriving (Show,Eq,Ord,Typeable,Data,Generic)

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  Pair x y <> Pair x' y' = Pair (x <> x') (y <> y')
  {-# INLINABLE (<>) #-}

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty  = Pair mempty mempty
  mappend = (<>)
  {-# INLINABLE mempty  #-}
  {-# INLINABLE mappend #-}

instance (StatMonoid a x, StatMonoid b x) => StatMonoid (Pair a b) x where
  addValue (Pair a b) !x = Pair (addValue a x) (addValue b x)
  singletonMonoid x = Pair (singletonMonoid x) (singletonMonoid x)
  {-# INLINE addValue        #-}
  {-# INLINE singletonMonoid #-}


-- | Strict pair for parallel accumulation
data PPair a b = PPair !a !b

instance (Semigroup a, Semigroup b) => Semigroup (PPair a b) where
  PPair x y <> PPair x' y' = PPair (x <> x') (y <> y')
  {-# INLINABLE (<>) #-}

instance (Monoid a, Monoid b) => Monoid (PPair a b) where
  mempty = PPair mempty mempty
  PPair x y `mappend` PPair x' y' = PPair (x `mappend` x') (y `mappend` y')
  {-# INLINABLE mempty  #-}
  {-# INLINABLE mappend #-}

instance (StatMonoid a x, StatMonoid b y) => StatMonoid (PPair a b) (x,y) where
  addValue (PPair a b) (!x,!y) = PPair (addValue a x) (addValue b y)
  singletonMonoid (!x,!y) = PPair (singletonMonoid x) (singletonMonoid y)
  {-# INLINE addValue        #-}
  {-# INLINE singletonMonoid #-}





derivingUnbox "Pair"
  [t| forall a b. (Unbox a, Unbox b) => Pair a b -> (a,b) |]
  [| \(Pair a b) -> (a,b) |]
  [| \(a,b) -> Pair a b   |]
