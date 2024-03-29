{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Module     : Data.Monoid.Statistics
-- Copyright  : Copyright (c) 2010,2017, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
module Data.Monoid.Statistics.Class
  ( -- * Monoid Type class and helpers
    StatMonoid(..)
  , reduceSample
  , reduceSampleVec
    -- * Ad-hoc type classes for select statistics
    -- $adhoc
  , CalcCount(..)
  , CalcNEvt(..)
  , CalcMean(..)
  , HasMean(..)
  , CalcVariance(..)
  , HasVariance(..)
    -- ** Deriving via
  , CalcViaHas(..)
    -- * Exception handling
  , Partial(..)
  , partial
  , SampleError(..)
    -- * Data types
  , Pair(..)
  ) where

import           Control.Exception
import           Control.Monad.Catch (MonadThrow(..))
import           Data.Data           (Typeable,Data)
import           Data.Monoid
import           Data.Int
import           Data.Word
import           Data.Vector.Unboxed          (Unbox)
import           Data.Vector.Unboxed.Deriving (derivingUnbox)
import qualified Data.Foldable       as F
import qualified Data.Vector.Generic as G
import           Numeric.Sum
import GHC.Stack    (HasCallStack)
import GHC.Generics (Generic)


-- | This type class is used to express parallelizable constant space
--   algorithms for calculation of statistics. /Statistic/ is function
--   of type @[a]→b@ which does not depend on order of elements. (for
--   example: mean, sum, number of elements, variance, etc).
--
--   For many statistics it's possible to possible to construct
--   constant space algorithm which is expressed as fold. Additionally
--   it's usually possible to write function which combine state of
--   fold accumulator to get statistic for union of two samples.
--
--   Thus for such algorithm we have value which corresponds to empty
--   sample, function which which corresponds to merging of two
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
  addValue m a = m <> singletonMonoid a
  {-# INLINE addValue #-}
  -- | State of accumulator corresponding to 1-element sample.
  singletonMonoid :: a -> m
  singletonMonoid = addValue mempty
  {-# INLINE singletonMonoid #-}
  {-# MINIMAL addValue | singletonMonoid #-}

-- | Calculate statistic over 'Foldable'. It's implemented in terms of
--   foldl'. Note that in cases when accumulator is immediately
--   consumed by polymorphic function such as 'callMeam' its type
--   becomes ambiguous. @TypeApplication@ then could be used to
--   disambiguate.
--
-- >>> reduceSample @Mean [1,2,3,4]
-- MeanKBN 4 (KBNSum 10.0 0.0)
-- >>> calcMean $ reduceSample @Mean [1,2,3,4] :: Maybe Double
-- Just 2.5
reduceSample :: forall m a f. (StatMonoid m a, F.Foldable f) => f a -> m
reduceSample = F.foldl' addValue mempty

-- | Calculate statistic over vector. Works in same was as
-- 'reduceSample' but works for vectors.
reduceSampleVec :: forall m a v. (StatMonoid m a, G.Vector v a) => v a -> m
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

-- $adhoc
--
-- Type classes defined here allows to extract common statistics from
-- estimators. it's assumed that quantities in question are already
-- computed so extraction is cheap.
--
--
-- ==== Error handling
--
-- Computation of statistics may fail. For example mean is not defined
-- for an empty sample. @Maybe@ could be seen as easy way to handle
-- this situation. But in many cases most convenient way to handle
-- failure is to throw an exception. So failure is encoded by using
-- polymorphic function of type @MonadThrow m ⇒ a → m X@.
--
-- Maybe types has instance, such as 'Maybe', 'Either'
-- 'Control.Exception.SomeException', 'IO' and most transformers
-- wrapping it. Notably this library defines 'Partial' monad which
-- allows to convert failures to exception in pure setting.
--
-- >>> calcMean $ reduceSample @Mean []
-- *** Exception: EmptySample "Data.Monoid.Statistics.Numeric.MeanKBN: calcMean"
--
-- >>> calcMean $ reduceSample @Mean [] :: Maybe Double
-- Nothing
--
-- >>> import Control.Exception
-- >>> calcMean $ reduceSample @Mean [] :: Either SomeException Double
-- Left (EmptySample "Data.Monoid.Statistics.Numeric.MeanKBN: calcMean")
--
-- Last example uses IO
--
-- >>> calcMean $ reduceSample @Mean []
-- *** Exception: EmptySample "Data.Monoid.Statistics.Numeric.MeanKBN: calcMean"
--
--
-- ==== Deriving instances
--
-- Type classes come in two variants, one that allow failure and one
-- for use in cases when quantity is always defined. This is not the
-- case for estimators, but true for distributions and intended for
-- such use cases. In that case 'CalcViaHas' could be used to derive
-- necessary instances.
--
-- >>> :{
-- data NormalDist = NormalDist !Double !Double
--   deriving (CalcMean,CalcVariance) via CalcViaHas NormalDist
-- instance HasMean NormalDist where
--   getMean (NormalDist mu _) = mu
-- instance HasVariance NormalDist where
--   getVariance   (NormalDist _ s) = s
--   getVarianceML (NormalDist _ s) = s
-- :}


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
  --
  --   \[ \bar{x} = \frac{1}{N}\sum_{i=1}^N{x_i} \]
  calcMean :: MonadThrow m => a -> m Double

-- | Same as 'CalcMean' but should never fail
class CalcMean a => HasMean a where
  getMean :: a -> Double


-- | Values from which we can efficiently compute estimate of sample
--   variance or distribution variance. It has two methods: one which
--   applies bias correction to estimate and another that returns
--   maximul likelyhood estimate. For distribution they should return
--   same value.
class CalcVariance a where
  -- | /Assumed O(1)/ Calculate unbiased estimate of variance:
  --
  --   \[ \sigma^2 = \frac{1}{N-1}\sum_{i=1}^N(x_i - \bar{x})^2 \]
  calcVariance :: MonadThrow m => a -> m Double
  calcVariance = fmap (\x->x*x) . calcStddev
  -- | /Assumed O(1)/ Calculate maximum likelihood estimate of variance:
  --
  --   \[ \sigma^2 = \frac{1}{N}\sum_{i=1}^N(x_i - \bar{x})^2 \]
  calcVarianceML :: MonadThrow m => a -> m Double
  calcVarianceML = fmap (\x->x*x) . calcStddevML
  -- | Calculate sample standard deviation from unbiased estimation of
  --   variance.
  calcStddev :: MonadThrow m => a -> m Double
  calcStddev = fmap sqrt . calcVariance
  -- | Calculate sample standard deviation from maximum likelihood
  --   estimation of variance.
  calcStddevML :: (MonadThrow m) => a -> m Double
  calcStddevML = fmap sqrt . calcVarianceML
  {-# MINIMAL (calcVariance,calcVarianceML) | (calcStddev,calcStddevML) #-}

-- | Same as 'CalcVariance' but never fails
class CalcVariance a => HasVariance a where
  getVariance   :: a -> Double
  getVariance   = (\x -> x*x) . getStddev
  getVarianceML :: a -> Double
  getVarianceML = (\x -> x*x) . getStddevML
  getStddev     :: a -> Double
  getStddev     = sqrt . getVariance
  getStddevML   :: a -> Double
  getStddevML   = sqrt . getVarianceML
  {-# MINIMAL (getVariance,getVarianceML) | (getStddev,getStddevML) #-}


-- | Type class for accumulators that are used for event counting with
--   possibly weighted events. Those are mostly used as accumulators
--   in histograms.
class CalcNEvt a where
  -- | Calculate sum of events weights.
  calcEvtsW :: a -> Double
  -- | Calculate error estimate (1σ or 68% CL). All instances defined
  --   in library use normal approximation which breaks down for small
  --   number of events.
  calcEvtsWErr :: a -> Double
  -- | Calculate effective number of events which is defined as
  --   \[N=E(w)^2/\operatorname{Var}(w)\] or as number of events
  --   which will yield same estimate for mean variance is they all
  --   have same weight.
  calcEffNEvt :: a -> Double
  calcEffNEvt = calcEvtsW

instance CalcNEvt Int where
  calcEvtsW    = fromIntegral
  calcEvtsWErr = sqrt . calcEvtsW

instance CalcNEvt Int32 where
  calcEvtsW    = fromIntegral
  calcEvtsWErr = sqrt . calcEvtsW

instance CalcNEvt Int64 where
  calcEvtsW    = fromIntegral
  calcEvtsWErr = sqrt . calcEvtsW

instance CalcNEvt Word where
  calcEvtsW    = fromIntegral
  calcEvtsWErr = sqrt . calcEvtsW

instance CalcNEvt Word32 where
  calcEvtsW    = fromIntegral
  calcEvtsWErr = sqrt . calcEvtsW

instance CalcNEvt Word64 where
  calcEvtsW    = fromIntegral
  calcEvtsWErr = sqrt . calcEvtsW


-- | Derive instances for 'CalcMean' and 'CalcVariance' from 'HasMean'
--   and 'HasVariance' instances.
newtype CalcViaHas a = CalcViaHas a
  deriving newtype (HasMean, HasVariance)

instance HasMean a => CalcMean (CalcViaHas a) where
  calcMean = pure . getMean

instance HasVariance a => CalcVariance (CalcViaHas a) where
  calcVariance   = pure . getVariance
  calcVarianceML = pure . getVarianceML

----------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------

-- | Identity monad which is used to encode partial functions for
--   'MonadThrow' based error handling. Its @MonadThrow@ instance
--   just throws normal exception.
newtype Partial a = Partial a
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

-- | Convert error to IO exception. This way one could for example
--   convert case when some statistics is not defined to an exception:
--
-- >>> calcMean $ reduceSample @Mean []
-- *** Exception: EmptySample "Data.Monoid.Statistics.Numeric.MeanKBN: calcMean"
partial :: HasCallStack => Partial a -> a
partial (Partial x) = x

instance Functor Partial where
  fmap f (Partial a) = Partial (f a)

instance Applicative Partial where
  pure = Partial
  Partial f <*> Partial a = Partial (f a)
  (!_) *> a   = a
  a   <* (!_) = a
instance Monad Partial where
  return = pure
  Partial a >>= f = f a
  (>>) = (*>)

instance MonadThrow Partial where
  throwM = throw

-- | Exception which is thrown when we can't compute some value
data SampleError
  = EmptySample String
  -- ^ @EmptySample function@: We're trying to compute quantity that
  --   is undefined for empty sample.
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
  mempty  = PPair mempty mempty
  mappend = (<>)
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

-- $setup
--
-- >>> :set -XDerivingVia
-- >>> import Data.Monoid.Statistics.Numeric
