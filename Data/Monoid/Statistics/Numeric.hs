{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Monoid.Statistics.Numeric ( Mean(..)
                                      , Variance()
                                      , calcCountVar
                                      , calcMeanVar
                                      , calcVariance
                                      , calcVarianceUnbiased
                                      , calcStddev
                                      , calcStddevUnbiased
                                        -- * Conversion to Double
                                      , ConvertibleToDouble(..)
                                      ) where

import Data.Int     (Int8, Int16, Int32, Int64)
import Data.Word    (Word8,Word16,Word32,Word64,Word)
import GHC.Float    (float2Double)

import Data.Monoid
import Data.Monoid.Statistics


----------------------------------------------------------------
-- Statistical monoids
----------------------------------------------------------------

-- | Mean of sample. Samples of Double,Float and bui;t-in integral
--   types are supported
--
-- Numeric stability of 'mappend' is not proven.
data Mean = Mean { calcMean      :: {-# UNPACK #-} !Double -- ^ Current mean
                 , calcCountMean :: {-# UNPACK #-} !Int    -- ^ Number of entries
                 }
            deriving Show

instance Monoid Mean where
  mempty = Mean 0 0
  mappend !(Mean x n) !(Mean y k) = Mean ((x*n' + y*k') / (n' + k')) (n + k)
    where
      n' = fromIntegral n
      k' = fromIntegral k
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

instance ConvertibleToDouble a => StatMonoid Mean a where
  pappend !x !(Mean m n) = Mean (m + (toDouble x - m) / fromIntegral n') n' where n' = n+1
  {-# INLINE pappend #-}


-- | Intermediate quantities to calculate the standard deviation.
data Variance = Variance { varianceSum   :: {-# UNPACK #-} !Double
                     -- ^ Current sum of elements of sample
                   , varianceSumSq :: {-# UNPACK #-} !Double
                     -- ^ Current sum of squares of deviations from current mean
                   , calcCountVar  :: {-# UNPACK #-} !Int
                     -- ^ Number of elements in the sample
                   }
           deriving Show

-- | Calculate mean of the sample (use 'Mean' if you need only it).
calcMeanVar :: Variance -> Double
calcMeanVar s = varianceSum s / fromIntegral (calcCountVar s)
{-# INLINE calcMeanVar #-}

-- | Calculate biased estimate of variance
calcVariance :: Variance -> Double
calcVariance s = varianceSumSq s / fromIntegral (calcCountVar s)
{-# INLINE calcVariance #-}

-- | Calculate unbiased estimate of the variance, where the
--   denominator is $n-1$.
calcVarianceUnbiased :: Variance -> Double
calcVarianceUnbiased s = varianceSumSq s / fromIntegral (calcCountVar s - 1)
{-# INLINE calcVarianceUnbiased #-}

-- | Calculate sample standard deviation (biased estimator, $s$, where
--   the denominator is $n-1$).
calcStddev :: Variance -> Double
calcStddev = sqrt . calcVariance
{-# INLINE calcStddev #-}

-- | Calculate standard deviation of the sample
-- (unbiased estimator, $\sigma$, where the denominator is $n$).
calcStddevUnbiased :: Variance -> Double
calcStddevUnbiased = sqrt . calcVarianceUnbiased
{-# INLINE calcStddevUnbiased #-}


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
  mappend !(Variance ta sa n1) !(Variance tb sb n2) = Variance (ta+tb) sumsq (n1+n2)
    where
      na = fromIntegral n1
      nb = fromIntegral n2
      nom = (ta * nb - tb * na)^2
      sumsq
        | n1 == 0 || n2 == 0 = sa + sb  -- because either sa or sb should be 0
        | otherwise          = sa + sb + nom / ((na + nb) * na * nb)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

instance ConvertibleToDouble a => StatMonoid Variance a where
  -- Can be implemented directly as in Welford-Knuth algorithm.
  pappend !x !s = s `mappend` (Variance (toDouble x) 0 1)
  {-# INLINE pappend #-}



----------------------------------------------------------------
-- Conversion to Double
----------------------------------------------------------------

-- | Data type which could be convered to Double
class ConvertibleToDouble a where
  toDouble :: a -> Double
  
-- Floating point
instance ConvertibleToDouble Double where
  toDouble = id
  {-# INLINE toDouble #-}
instance ConvertibleToDouble Float where
  toDouble = float2Double
  {-# INLINE toDouble #-}
-- Basic integral types
instance ConvertibleToDouble Integer where
  toDouble = fromIntegral
  {-# INLINE toDouble #-}
instance ConvertibleToDouble Int where
  toDouble = fromIntegral
  {-# INLINE toDouble #-}
instance ConvertibleToDouble Word where
  toDouble = fromIntegral
  {-# INLINE toDouble #-}
-- Integral types with fixed size
instance ConvertibleToDouble Int8 where
  toDouble = fromIntegral
  {-# INLINE toDouble #-}
instance ConvertibleToDouble Int16 where
  toDouble = fromIntegral
  {-# INLINE toDouble #-}
instance ConvertibleToDouble Int32 where
  toDouble = fromIntegral
  {-# INLINE toDouble #-}
instance ConvertibleToDouble Int64 where
  toDouble = fromIntegral
  {-# INLINE toDouble #-}
instance ConvertibleToDouble Word8 where
  toDouble = fromIntegral
  {-# INLINE toDouble #-}
instance ConvertibleToDouble Word16 where
  toDouble = fromIntegral
  {-# INLINE toDouble #-}
instance ConvertibleToDouble Word32 where
  toDouble = fromIntegral
  {-# INLINE toDouble #-}
instance ConvertibleToDouble Word64 where
  toDouble = fromIntegral
  {-# INLINE toDouble #-}

 