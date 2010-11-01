{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Monoid.Statistics.Numeric ( Mean(..)
                                      , Stdev()
                                      , stdev
                                      , stdevUnbiased
                                      , variance
                                      , mean
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
-- Only samples of 'Double' are supported.
data Stdev = Stdev { stdev'sum   :: {-# UNPACK #-} !Double
                                 -- ^ Current sum $\sum_i x_i$.
                   , stdev'sumsq :: {-# UNPACK #-} !Double
                                 -- ^ Current $\sum_i (x_i - \bar {x_n})^2$.
                   , stdev'n     :: {-# UNPACK #-} !Int
                                 -- ^ Current length of the sample.
                   }
           deriving Show

-- | Calculate mean of the sample (use 'Mean' if you need only it).
mean :: Stdev -> Double
mean !s = stdev'sum s / fromIntegral (stdev'n s)

-- | Calculate standard deviation of the sample
-- (unbiased estimator, $\sigma$, where the denominator is $n$).
stdevUnbiased :: Stdev -> Double
stdevUnbiased !s = sqrt $ m2 / n
  where n = fromIntegral $ stdev'n s
        m2 = stdev'sumsq s

-- | Calculate sample standard deviation (biased estimator, $s$, where
-- the denominator is $n-1$).
stdev :: Stdev -> Double
stdev = sqrt . variance

-- | Calculate unbiased estimate of the variance, where the
-- denominator is $n-1$.
variance :: Stdev -> Double
variance !s = m2 / (n-1)
  where n = fromIntegral $ stdev'n s
        m2 = stdev'sumsq s

-- | Using parallel algorithm from:
-- 
-- Chan, Tony F.; Golub, Gene H.; LeVeque, Randall J. (1979),
-- Updating Formulae and a Pairwise Algorithm for Computing Sample
-- Variances., Technical Report STAN-CS-79-773, Department of
-- Computer Science, Stanford University. Page 4.
-- 
-- <ftp://reports.stanford.edu/pub/cstr/reports/cs/tr/79/773/CS-TR-79-773.pdf>
--
instance Monoid Stdev where
  mempty = Stdev 0 0 0
  mappend !(Stdev ta sa n1) !(Stdev tb sb n2) = Stdev (ta+tb) sumsq (n1+n2)
    where
      na = fromIntegral n1
      nb = fromIntegral n2
      nom = (ta * nb - tb * na)^2
      sumsq
        | n1 == 0 || n2 == 0 = sa + sb  -- because either sa or sb should be 0
        | otherwise          = sa + sb + nom / ((na + nb) * na * nb)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

instance ConvertibleToDouble a => StatMonoid Stdev a where
  -- Can be implemented directly as in Welford-Knuth algorithm.
  pappend !x !s = s `mappend` (Stdev (toDouble x) 0 1)
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

 