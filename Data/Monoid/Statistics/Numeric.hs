{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Monoid.Statistics.Numeric (
    -- * Mean & Variance
    -- ** Number of elements
    CountG(..)
  , Count
  , asCount
    -- ** Mean
  , Mean(..)
  , asMean
    -- ** Variance
  , Variance(..)
  , asVariance
    -- * Maximum and minimum
  , Max(..)
  , Min(..)
  , MaxD(..)
  , MinD(..)
    -- * Accessors
    -- $accessors
  , CalcCount(..)
  , CalcMean(..)
  , CalcVariance(..)
  , calcStddev
  , calcStddevML
  ) where

import Data.Monoid
import Data.Monoid.Statistics.Class
import Data.Data (Typeable,Data)
import GHC.Generics (Generic)

----------------------------------------------------------------
-- Statistical monoids
----------------------------------------------------------------

-- | Simplest statistics. Number of elements in the sample
newtype CountG a = CountG { calcCountN :: a }
                  deriving (Show,Eq,Ord,Typeable)

type Count = CountG Int

-- | Type restricted 'id'
asCount :: CountG a -> CountG a
asCount = id

instance Integral a => Monoid (CountG a) where
  mempty                      = CountG 0
  CountG i `mappend` CountG j = CountG (i + j)
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

instance (Integral a) => StatMonoid (CountG a) b where
  singletonMonoid _            = CountG 1
  addValue        (CountG n) _ = CountG (n + 1)
  {-# INLINE singletonMonoid #-}
  {-# INLINE addValue        #-}

instance CalcCount (CountG Int) where
  calcCount = calcCountN
  {-# INLINE calcCount #-}



----------------------------------------------------------------

-- | Mean of sample. Samples of Double,Float and bui;t-in integral
--   types are supported
--
-- Numeric stability of 'mappend' is not proven.
data Mean = Mean !Int    -- Number of entries
                 !Double -- Current mean
            deriving (Show,Eq,Typeable,Data,Generic)

-- | Type restricted 'id'
asMean :: Mean -> Mean
asMean = id

instance Monoid Mean where
  mempty = Mean 0 0
  mappend (Mean n x) (Mean k y) = Mean (n + k) ((x*n' + y*k') / (n' + k'))
    where
      n' = fromIntegral n
      k' = fromIntegral k
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

instance Real a => StatMonoid Mean a where
  addValue (Mean n m) !x = Mean n' (m + (realToFrac x - m) / fromIntegral n') where n' = n+1
  {-# INLINE addValue #-}

instance CalcCount Mean where
  calcCount (Mean n _) = n
instance CalcMean Mean where
  calcMean (Mean 0 _) = Nothing
  calcMean (Mean _ m) = Just m



----------------------------------------------------------------

-- | Intermediate quantities to calculate the standard deviation.
data Variance = Variance {-# UNPACK #-} !Int    --  Number of elements in the sample
                         {-# UNPACK #-} !Double -- Current sum of elements of sample
                         {-# UNPACK #-} !Double -- Current sum of squares of deviations from current mean
                deriving (Show,Eq,Typeable)

-- | Fix type of monoid
asVariance :: Variance -> Variance
asVariance = id
{-# INLINE asVariance #-}

-- | Using parallel algorithm from:
--
-- Chan, Tony F.; Golub, Gene H.; LeVeque, Randall J. (1979),
-- Updating Formulae and a Pairwise Algorithm for Computing Sample
-- Variances., Technical Report STAN-CS-79-773, Department of
-- Computer Science, Stanford University. Page 4.
--
-- <ftp://reports.stanford.edu/pub/cstr/reports/cs/tr/79/773/CS-TR-79-773.pdf>
--
instance Monoid Variance where
  mempty = Variance 0 0 0
  mappend (Variance n1 ta sa) (Variance n2 tb sb) = Variance (n1+n2) (ta+tb) sumsq
    where
      na = fromIntegral n1
      nb = fromIntegral n2
      nom = sqr (ta * nb - tb * na)
      sumsq
        | n1 == 0 || n2 == 0 = sa + sb  -- because either sa or sb should be 0
        | otherwise          = sa + sb + nom / ((na + nb) * na * nb)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

instance Real a => StatMonoid Variance a where
  singletonMonoid x = Variance 1 (realToFrac x) 0
  {-# INLINE singletonMonoid #-}

instance CalcCount Variance where
  calcCount (Variance n _ _) = n

instance CalcMean Variance where
  calcMean (Variance 0 _ _) = Nothing
  calcMean (Variance n s _) = Just (s / fromIntegral n)

instance CalcVariance Variance where
  calcVariance (Variance n _ s)
    | n < 2     = Nothing
    | otherwise = Just $! s / fromIntegral (n - 1)
  calcVarianceML (Variance n _ s)
    | n < 1     = Nothing
    | otherwise = Just $! s / fromIntegral n




----------------------------------------------------------------

-- | Calculate minimum of sample
newtype Min a = Min { calcMin :: Maybe a }

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  Min (Just a) `mappend` Min (Just b) = Min (Just $! min a b)
  Min a        `mappend` Min Nothing  = Min a
  Min Nothing  `mappend` Min b        = Min b

instance (Ord a, a ~ a') => StatMonoid (Min a) a' where
  singletonMonoid a = Min (Just a)

----------------------------------------------------------------

-- | Calculate maximum of sample
newtype Max a = Max { calcMax :: Maybe a }

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  Max (Just a) `mappend` Max (Just b) = Max (Just $! min a b)
  Max a        `mappend` Max Nothing  = Max a
  Max Nothing  `mappend` Max b        = Max b

instance (Ord a, a ~ a') => StatMonoid (Max a) a' where
  singletonMonoid a = Max (Just a)


----------------------------------------------------------------

-- | Calculate minimum of sample of Doubles. For empty sample returns NaN. Any
--   NaN encountered will be ignored.
newtype MinD = MinD { calcMinD :: Double }
              deriving (Show,Eq,Ord,Typeable,Data,Generic)

-- N.B. forall (x :: Double) (x <= NaN) == False
instance Monoid MinD where
  mempty = MinD (0/0)
  mappend (MinD x) (MinD y)
    | isNaN x   = MinD y
    | isNaN y   = MinD x
    | otherwise = MinD (min x y)
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

instance a ~ Double => StatMonoid MinD a where
  singletonMonoid = MinD

-- | Calculate maximum of sample. For empty sample returns NaN. Any
--   NaN encountered will be ignored.
newtype MaxD = MaxD { calcMaxD :: Double }
              deriving (Show,Eq,Ord,Typeable,Data,Generic)

instance Monoid MaxD where
  mempty = MaxD (0/0)
  mappend (MaxD x) (MaxD y)
    | isNaN x   = MaxD y
    | isNaN y   = MaxD x
    | otherwise = MaxD (max x y)
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

instance a ~ Double => StatMonoid MaxD a where
  singletonMonoid = MaxD





----------------------------------------------------------------
-- Ad-hoc type class
----------------------------------------------------------------

-- | Accumulator could be used to evaluate number of elements in
--   sample.
class CalcCount m where
  -- | Number of elements in sample
  calcCount :: m -> Int

-- | Monoids which could be used to calculate sample mean:
--
--   \[ \bar{x} = \frac{1}{N}\sum_{i=1}^N{x_i} \]
class CalcMean m where
  -- | Returns @Nothing@ if there isn't enough data to make estimate.
  calcMean :: m -> Maybe Double

-- | Monoids which could be used to calculate sample variance. Both
--   methods return @Nothing@ if there isn't enough data to make
--   estimate.
class CalcVariance m where
  -- | Calculate unbiased estimate of variance:
  --
  --   \[ \sigma^2 = \frac{1}{N-1}\sum_{i=1}^N(x_i - \bar{x})^2 \]
  calcVariance   :: m -> Maybe Double
  -- | Calculate maximum likelihood estimate of variance:
  --
  --   \[ \sigma^2 = \frac{1}{N}\sum_{i=1}^N(x_i - \bar{x})^2 \]
  calcVarianceML :: m -> Maybe Double

-- | Calculate sample standard deviation from unbiased estimation of
--   variance:
--
--   \[ \sigma = \sqrt{\frac{1}{N-1}\sum_{i=1}^N(x_i - \bar{x})^2 } \]
calcStddev :: CalcVariance m => m -> Maybe Double
calcStddev = fmap sqrt . calcVariance

-- | Calculate sample standard deviation from maximum likelihood
--   estimation of variance:
--
--   \[ \sigma = \sqrt{\frac{1}{N-1}\sum_{i=1}^N(x_i - \bar{x})^2 } \]
calcStddevML :: CalcVariance m => m -> Maybe Double
calcStddevML = fmap sqrt . calcVarianceML



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

sqr :: Double -> Double
sqr x = x * x
{-# INLINE sqr #-}
