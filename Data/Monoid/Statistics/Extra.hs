{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Monoids for calculating various statistics in constant space. This
-- module contains algorithms that should be generally avoided unless
-- there's specific reason to use them.
module Data.Monoid.Statistics.Extra (
    -- * Mean
    WelfordMean(..)
  , asWelfordMean
  , MeanKahan(..)
  , asMeanKahan
  , MeanKB2(..)
  , asMeanKB2
    -- $references
  ) where

import Control.Monad.Catch          (MonadThrow(..))
import Data.Data                    (Typeable,Data)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Numeric.Sum
import GHC.Generics                 (Generic)

import Data.Monoid.Statistics.Class



----------------------------------------------------------------
-- Mean
----------------------------------------------------------------


-- | Incremental calculation of mean which uses second-order
--   compensated Kahan-Babuška summation. In most cases
--   'Data.Monoid.Statistics.Numeric.KBNSum' should provide enough
--   precision.
data MeanKB2 = MeanKB2 !Int {-# UNPACK #-} !KB2Sum
             deriving (Show,Eq)

asMeanKB2 :: MeanKB2 -> MeanKB2
asMeanKB2 = id

instance Semigroup MeanKB2 where
  MeanKB2 0  _  <> m             = m
  m             <> MeanKB2 0  _  = m
  MeanKB2 n1 s1 <> MeanKB2 n2 s2 = MeanKB2 (n1+n2) (s1 <> s2)

instance Monoid MeanKB2 where
  mempty  = MeanKB2 0 mempty
  mappend = (<>)

instance Real a => StatMonoid MeanKB2 a where
  addValue (MeanKB2 n m) x = MeanKB2 (n+1) (addValue m x)

instance CalcMean MeanKB2 where
  calcMean (MeanKB2 0 _) = throwM $ EmptySample "Data.Monoid.Statistics.Extra.MeanKB2"
  calcMean (MeanKB2 n s) = return $! kb2 s / fromIntegral n



-- | Incremental calculation of mean. Sum of elements is calculated
--   using compensated Kahan summation. It's provided only for sake of
--   completeness. 'Data.Monoid.Statistics.Numeric.KBNSum' should be used
--   instead.
data MeanKahan = MeanKahan !Int !KahanSum
             deriving (Show,Eq,Typeable,Data,Generic)

asMeanKahan :: MeanKahan -> MeanKahan
asMeanKahan = id


instance Semigroup MeanKahan where
  MeanKahan 0  _  <> m               = m
  m               <> MeanKahan 0  _  = m
  MeanKahan n1 s1 <> MeanKahan n2 s2 = MeanKahan (n1+n2) (s1 <> s2)
  {-# INLINE (<>) #-}

instance Monoid MeanKahan where
  mempty  = MeanKahan 0 mempty
  mappend = (<>)

instance Real a => StatMonoid MeanKahan a where
  addValue (MeanKahan n m) x = MeanKahan (n+1) (addValue m x)

instance CalcCount MeanKahan where
  calcCount (MeanKahan n _) = n
instance CalcMean MeanKahan where
  calcMean (MeanKahan 0 _) = throwM $ EmptySample "Data.Monoid.Statistics.Extra.WelfordMean"
  calcMean (MeanKahan n s) = return (kahan s / fromIntegral n)


-- | Incremental calculation of mean. Note that this algorithm doesn't
--   offer better numeric precision than plain summation. Its only
--   advantage is protection against double overflow:
--
-- >>> calcMean $ reduceSample @MeanKBN (replicate 100 1e308) :: Maybe Double
-- Just NaN
-- >>> calcMean $ reduceSample @WelfordMean (replicate 100 1e308) :: Maybe Double
-- Just 1.0e308
--
--   Unless this feature is needed 'Data.Monoid.Statistics.Numeric.KBNSum'
--   should be used. Algorithm is due to Welford [Welford1962]
--
-- \[ s_n = s_{n-1} + \frac{x_n - s_{n-1}}{n} \]
data WelfordMean = WelfordMean !Int    -- Number of entries
                               !Double -- Current mean
  deriving (Show,Eq,Typeable,Data,Generic)

-- | Type restricted 'id'
asWelfordMean :: WelfordMean -> WelfordMean
asWelfordMean = id

instance Semigroup WelfordMean where
  WelfordMean 0 _ <> m = m
  m <> WelfordMean 0 _ = m
  WelfordMean n x <> WelfordMean k y
    = WelfordMean (n + k) ((x*n' + y*k') / (n' + k'))
    where
      n' = fromIntegral n
      k' = fromIntegral k
  {-# INLINE (<>) #-}

instance Monoid WelfordMean where
  mempty  = WelfordMean 0 0
  mappend = (<>)
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

instance Real a => StatMonoid WelfordMean a where
  addValue (WelfordMean n m) !x
    = WelfordMean n' (m + (realToFrac x - m) / fromIntegral n')
    where
      n' = n+1
  {-# INLINE addValue #-}

instance CalcCount WelfordMean where
  calcCount (WelfordMean n _) = n
instance CalcMean WelfordMean where
  calcMean (WelfordMean 0 _) = throwM $ EmptySample "Data.Monoid.Statistics.Extra.WelfordMean"
  calcMean (WelfordMean _ m) = return m



----------------------------------------------------------------
-- Unboxed instances
----------------------------------------------------------------

derivingUnbox "MeanKahan"
  [t| MeanKahan -> (Int,Double,Double) |]
  [| \(MeanKahan a (KahanSum b c)) -> (a,b,c)   |]
  [| \(a,b,c) -> MeanKahan a (KahanSum b c) |]

derivingUnbox "WelfordMean"
  [t| WelfordMean -> (Int,Double) |]
  [| \(WelfordMean a b) -> (a,b)  |]
  [| \(a,b) -> WelfordMean a b    |]


-- $references
--
-- * [Welford1962] Welford, B.P. (1962) Note on a method for
--   calculating corrected sums of squares and
--   products. /Technometrics/
--   4(3):419-420. <http://www.jstor.org/stable/1266577>

-- $setup
--
-- >>> :set -XTypeApplications
-- >>> import Data.Monoid.Statistics.Numeric
