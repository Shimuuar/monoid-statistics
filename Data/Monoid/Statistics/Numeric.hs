{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
-- Monoids for calculating various statistics in constant space
module Data.Monoid.Statistics.Numeric (
    -- * Mean & Variance
    -- ** Number of elements
    CountG(..)
  , Count
  , asCount
    -- ** Mean algorithms
    -- ** Default algorithms
  , Mean
  , asMean
  , WMean
  , asWMean
    -- *** Mean
  , MeanNaive(..)
  , asMeanNaive
  , MeanKBN(..)
  , asMeanKBN
    -- *** Weighted mean
  , WMeanNaive(..)
  , asWMeanNaive
  , WMeanKBN(..)
  , asWMeanKBN
    -- ** Variance
  , Variance(..)
  , asVariance
    -- * Maximum and minimum
  , Max(..)
  , Min(..)
  , MaxD(..)
  , MinD(..)
    -- * Binomial trials
  , BinomAcc(..)
  , asBinomAcc
    -- * Rest
  , Weighted(..)
    -- * References
    -- $references
  ) where

import Control.Monad.Catch          (MonadThrow(..))
import Data.Data                    (Typeable,Data)
import Data.Vector.Unboxed          (Unbox)
import Data.Vector.Unboxed          qualified as VU
import Data.Vector.Generic          qualified as VG
import Data.Vector.Generic.Mutable  qualified as VGM
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Numeric.Sum
import GHC.Generics                 (Generic)

import Data.Monoid.Statistics.Class


----------------------------------------------------------------
-- Statistical monoids
----------------------------------------------------------------

-- | Calculate number of elements in the sample.
newtype CountG a = CountG { calcCountN :: a }
  deriving stock   (Show,Eq,Ord,Data)

type Count = CountG Int

-- | Type restricted 'id'
asCount :: CountG a -> CountG a
asCount = id

instance Integral a => Semigroup (CountG a) where
  CountG i <> CountG j = CountG (i + j)

instance Integral a => Monoid (CountG a) where
  mempty  = CountG 0
  mappend = (<>)

instance (Integral a) => StatMonoid (CountG a) b where
  singletonMonoid _            = CountG 1
  addValue        (CountG n) _ = CountG (n + 1)


instance CalcCount (CountG Int) where
  calcCount = calcCountN

instance Real a => CalcNEvt (CountG a) where
  calcEvtsW    = realToFrac . calcCountN
  calcEvtsWErr = sqrt . calcEvtsW
  {-# INLINE calcEvtsW    #-}
  {-# INLINE calcEvtsWErr #-}

----------------------------------------------------------------

-- | Accumulator type for counting weighted events. Weights are
--   presumed to be independent and follow same distribution \[W\].
--   In this case sum of weights follows compound Poisson
--   distribution. Its expectation could be then estimated as
--   \[\sum_iw_i\] and variance as \[\sum_iw_i^2\].
--
--   Main use of this data type is as accumulator in histograms which
--   count weighted events.
data CountW = CountW
  !Double -- Sum of weights
  !Double -- Sum of weight squares
  deriving stock (Show,Eq,Generic)

instance Semigroup CountW where
  CountW wA w2A <> CountW wB w2B = CountW (wA+wB) (w2A+w2B)
  {-# INLINE (<>) #-}
instance Monoid CountW where
  mempty = CountW 0 0

instance Real a => StatMonoid CountW a where
  addValue (CountW w w2) a = CountW (w + x) (w2 + x*x)
    where
      x = realToFrac a

instance CalcNEvt CountW where
  calcEvtsW    (CountW w _ ) = w
  calcEvtsWErr (CountW _ w2) = sqrt w2
  calcEffNEvt  (CountW w w2) = w * w / w2

----------------------------------------------------------------

-- | Type alias for currently recommended algorithms for calculation
--   of mean. It should be default choice
type Mean = MeanKBN

asMean :: Mean -> Mean
asMean = id

-- | Type alias for currently recommended algorithms for calculation
--   of weighted mean. It should be default choice
type WMean = WMeanKBN

asWMean :: WMean -> WMean
asWMean = id


----------------------------------------------------------------

-- | Incremental calculation of mean. It tracks separately number of
--   elements and running sum. Note that summation of floating point
--   numbers loses precision and genrally use 'MeanKBN' is
--   recommended.
data MeanNaive = MeanNaive !Int !Double
  deriving stock (Show,Eq,Data,Generic)

asMeanNaive :: MeanNaive -> MeanNaive
asMeanNaive = id


instance Semigroup MeanNaive where
  MeanNaive 0  _  <> m               = m
  m               <> MeanNaive 0  _  = m
  MeanNaive n1 s1 <> MeanNaive n2 s2 = MeanNaive (n1+n2) (s1 + s2)

instance Monoid MeanNaive where
  mempty  = MeanNaive 0 0
  mappend = (<>)

instance Real a => StatMonoid MeanNaive a where
  addValue (MeanNaive n m) x = MeanNaive (n+1) (m + realToFrac x)
  {-# INLINE addValue #-}

instance CalcCount MeanNaive where
  calcCount (MeanNaive n _) = n
instance CalcMean MeanNaive where
  calcMean (MeanNaive 0 _) = throwM $ EmptySample "Data.Monoid.Statistics.Numeric.MeanNaive: calcMean"
  calcMean (MeanNaive n s) = return (s / fromIntegral n)


----------------------------------------------------------------

-- | Incremental calculation of mean. It tracks separately number of
--   elements and running sum. It uses algorithm for compensated
--   summation which works with mantissa of double size at cost of
--   doing more operations. This means that it's usually possible to
--   compute sum (and therefore mean) within 1 ulp.
data MeanKBN = MeanKBN !Int {-# UNPACK #-} !KBNSum
  deriving stock (Show,Eq,Data,Generic)

asMeanKBN :: MeanKBN -> MeanKBN
asMeanKBN = id


instance Semigroup MeanKBN where
  MeanKBN 0  _  <> m             = m
  m             <> MeanKBN 0  _  = m
  MeanKBN n1 s1 <> MeanKBN n2 s2 = MeanKBN (n1+n2) (s1 <> s2)

instance Monoid MeanKBN where
  mempty  = MeanKBN 0 mempty
  mappend = (<>)
  
instance Real a => StatMonoid MeanKBN a where
  addValue (MeanKBN n m) x = MeanKBN (n+1) (addValue m x)
  {-# INLINE addValue #-}

instance CalcCount MeanKBN where
  calcCount (MeanKBN n _) = n
instance CalcMean MeanKBN where
  calcMean (MeanKBN 0 _) = throwM $ EmptySample "Data.Monoid.Statistics.Numeric.MeanKBN: calcMean"
  calcMean (MeanKBN n s) = return (kbn s / fromIntegral n)


----------------------------------------------------------------

-- | Incremental calculation of weighed mean.
data WMeanNaive = WMeanNaive
  !Double  -- Weight
  !Double  -- Weighted sum
  deriving stock (Show,Eq,Data,Generic)

asWMeanNaive :: WMeanNaive -> WMeanNaive
asWMeanNaive = id


instance Semigroup WMeanNaive where
  WMeanNaive w1 s1 <> WMeanNaive w2 s2 = WMeanNaive (w1 + w2) (s1 + s2)

instance Monoid WMeanNaive where
  mempty  = WMeanNaive 0 0
  mappend = (<>)

instance (Real w, Real a) => StatMonoid WMeanNaive (Weighted w a) where
  addValue (WMeanNaive n s) (Weighted w a)
    = WMeanNaive (n + w') (s + (w' * a'))
    where
      w' = realToFrac w
      a' = realToFrac a
  {-# INLINE addValue #-}

instance CalcMean WMeanNaive where
  calcMean (WMeanNaive w s)
    | w <= 0    = throwM $ EmptySample "Data.Monoid.Statistics.Numeric.WMeanNaive: calcMean"
    | otherwise = return (s / w)

----------------------------------------------------------------

-- | Incremental calculation of weighed mean. Sum of both weights and
--   elements is calculated using Kahan-Babuška-Neumaier summation.
data WMeanKBN = WMeanKBN
  {-# UNPACK #-} !KBNSum  -- Weight
  {-# UNPACK #-} !KBNSum  -- Weighted sum
  deriving stock (Show,Eq,Data,Generic)

asWMeanKBN :: WMeanKBN -> WMeanKBN
asWMeanKBN = id


instance Semigroup WMeanKBN where
  WMeanKBN n1 s1 <> WMeanKBN n2 s2 = WMeanKBN (n1 <> n2) (s1 <> s2)

instance Monoid WMeanKBN where
  mempty  = WMeanKBN mempty mempty
  mappend = (<>)

instance (Real w, Real a) => StatMonoid WMeanKBN (Weighted w a) where
  addValue (WMeanKBN n m) (Weighted w a)
    = WMeanKBN (add n w') (add m (w' * a'))
    where
      w' = realToFrac w :: Double
      a' = realToFrac a :: Double
  {-# INLINE addValue #-}

instance CalcMean WMeanKBN where
  calcMean (WMeanKBN (kbn -> w) (kbn -> s))
    | w <= 0    = throwM $ EmptySample "Data.Monoid.Statistics.Numeric.WMeanKBN: calcMean"
    | otherwise = return (s / w)


----------------------------------------------------------------

-- | This is algorithm for estimation of mean and variance of sample
--   which uses modified Welford algorithm. It uses KBN summation and
--   provides approximately 2 additional decimal digits
data VarWelfordKBN = VarWelfordKBN
  {-# UNPACK #-} !Int    --  Number of elements in the sample
  {-# UNPACK #-} !KBNSum -- Current sum of elements of sample
  {-# UNPACK #-} !KBNSum -- Current sum of squares of deviations from current mean

asVarWelfordKBN :: VarWelfordKBN -> VarWelfordKBN
asVarWelfordKBN = id


-- | Incremental algorithms for calculation the standard deviation [Chan1979].
data Variance = Variance {-# UNPACK #-} !Int    --  Number of elements in the sample
                         {-# UNPACK #-} !Double -- Current sum of elements of sample
                         {-# UNPACK #-} !Double -- Current sum of squares of deviations from current mean
  deriving stock (Show,Eq,Typeable)

-- | Type restricted 'id '
asVariance :: Variance -> Variance
asVariance = id

instance Semigroup Variance where
  Variance n1 ta sa <> Variance n2 tb sb
    = Variance (n1+n2) (ta+tb) sumsq
    where
      na = fromIntegral n1
      nb = fromIntegral n2
      nom = sqr (ta * nb - tb * na)
      sumsq | n1 == 0   = sb
            | n2 == 0   = sa
            | otherwise = sa + sb + nom / ((na + nb) * na * nb)

instance Monoid Variance where
  mempty  = Variance 0 0 0
  mappend = (<>)

instance Real a => StatMonoid Variance a where
  addValue (Variance 0 _ _) x = singletonMonoid x
  addValue (Variance n t s) (realToFrac -> x)
    = Variance (n + 1) (t + x) (s + sqr (t  - n' * x) / (n' * (n'+1)))
    where
      n' = fromIntegral n
  {-# INLINE addValue #-}
  singletonMonoid x = Variance 1 (realToFrac x) 0
  {-# INLINE singletonMonoid #-}

instance CalcCount Variance where
  calcCount (Variance n _ _) = n

instance CalcMean Variance where
  calcMean (Variance 0 _ _) = throwM $ EmptySample "Data.Monoid.Statistics.Numeric.Variance: calcMean"
  calcMean (Variance n s _) = return (s / fromIntegral n)

instance CalcVariance Variance where
  calcVariance (Variance n _ s)
    | n < 2     = throwM $ InvalidSample
                    "Data.Monoid.Statistics.Numeric.Variance: calcVariance"
                    "Need at least 2 elements"
    | otherwise = return $! s / fromIntegral (n - 1)
  calcVarianceML (Variance n _ s)
    | n < 1     = throwM $ InvalidSample
                    "Data.Monoid.Statistics.Numeric.Variance: calcVarianceML"
                    "Need at least 1 element"
    | otherwise = return $! s / fromIntegral n



----------------------------------------------------------------

-- | Calculate minimum of sample
newtype Min a = Min { calcMin :: Maybe a }
  deriving stock (Show,Eq,Ord,Data,Generic)

instance Ord a => Semigroup (Min a) where
  Min (Just a) <> Min (Just b) = Min (Just $! min a b)
  Min a        <> Min Nothing  = Min a
  Min Nothing  <> Min b        = Min b

instance Ord a => Monoid (Min a) where
  mempty  = Min Nothing
  mappend = (<>)

instance (Ord a, a ~ a') => StatMonoid (Min a) a' where
  singletonMonoid a = Min (Just a)


----------------------------------------------------------------

-- | Calculate maximum of sample
newtype Max a = Max { calcMax :: Maybe a }
  deriving stock (Show,Eq,Ord,Data,Generic)

instance Ord a => Semigroup (Max a) where
  Max (Just a) <> Max (Just b) = Max (Just $! max a b)
  Max a        <> Max Nothing  = Max a
  Max Nothing  <> Max b        = Max b

instance Ord a => Monoid (Max a) where
  mempty  = Max Nothing
  mappend = (<>)

instance (Ord a, a ~ a') => StatMonoid (Max a) a' where
  singletonMonoid a = Max (Just a)


----------------------------------------------------------------

-- | Calculate minimum of sample of Doubles. For empty sample returns NaN. Any
--   NaN encountered will be ignored.
newtype MinD = MinD { calcMinD :: Double }
  deriving stock (Show,Data,Generic)

instance Eq MinD where
  MinD a == MinD b
    | isNaN a && isNaN b = True
    | otherwise          = a == b

instance Semigroup MinD where
  MinD x <> MinD y
    | isNaN x   = MinD y
    | isNaN y   = MinD x
    | otherwise = MinD (min x y)

-- N.B. forall (x :: Double) (x <= NaN) == False
instance Monoid MinD where
  mempty  = MinD (0/0)
  mappend = (<>)

instance a ~ Double => StatMonoid MinD a where
  singletonMonoid = MinD



-- | Calculate maximum of sample. For empty sample returns NaN. Any
--   NaN encountered will be ignored.
newtype MaxD = MaxD { calcMaxD :: Double }
  deriving stock (Show,Data,Generic)

instance Eq MaxD where
  MaxD a == MaxD b
    | isNaN a && isNaN b = True
    | otherwise          = a == b

instance Semigroup MaxD where
  MaxD x <> MaxD y
    | isNaN x   = MaxD y
    | isNaN y   = MaxD x
    | otherwise = MaxD (max x y)

instance Monoid MaxD where
  mempty  = MaxD (0/0)
  mappend = (<>)

instance a ~ Double => StatMonoid MaxD a where
  singletonMonoid = MaxD


----------------------------------------------------------------

-- | Accumulator for binomial trials.
data BinomAcc = BinomAcc { binomAccSuccess :: !Int
                         , binomAccTotal   :: !Int
                         }
  deriving stock (Show,Eq,Ord,Data,Generic)

-- | Type restricted 'id'
asBinomAcc :: BinomAcc -> BinomAcc
asBinomAcc = id

instance Semigroup BinomAcc where
  BinomAcc n1 m1 <> BinomAcc n2 m2 = BinomAcc (n1+n2) (m1+m2)

instance Monoid BinomAcc where
  mempty  = BinomAcc 0 0
  mappend = (<>)

instance StatMonoid BinomAcc Bool where
  addValue (BinomAcc nS nT) True  = BinomAcc (nS+1) (nT+1)
  addValue (BinomAcc nS nT) False = BinomAcc  nS    (nT+1)


-- | Value @a@ weighted by weight @w@
data Weighted w a = Weighted w a
  deriving stock (Show,Eq,Ord,Data,Generic,Functor,Foldable,Traversable)



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

sqr :: Double -> Double
sqr x = x * x
{-# INLINE sqr #-}


----------------------------------------------------------------
-- Unboxed instances
----------------------------------------------------------------

derivingUnbox "CountG"
  [t| forall a. Unbox a => CountG a -> a |]
  [| calcCountN |]
  [| CountG     |]

derivingUnbox "MeanNaive"
  [t| MeanNaive -> (Int,Double) |]
  [| \(MeanNaive a b) -> (a,b)  |]
  [| \(a,b) -> MeanNaive a b    |]

derivingUnbox "MeanKBN"
  [t| MeanKBN -> (Int,Double,Double)      |]
  [| \(MeanKBN a (KBNSum b c)) -> (a,b,c) |]
  [| \(a,b,c) -> MeanKBN a (KBNSum b c)   |]

derivingUnbox "WMeanNaive"
  [t| WMeanNaive -> (Double,Double) |]
  [| \(WMeanNaive a b) -> (a,b)     |]
  [| \(a,b) -> WMeanNaive a b       |]

derivingUnbox "WMeanKBN"
  [t| WMeanKBN -> (Double,Double,Double,Double)         |]
  [| \(WMeanKBN (KBNSum a b) (KBNSum c d)) -> (a,b,c,d) |]
  [| \(a,b,c,d) -> WMeanKBN (KBNSum a b) (KBNSum c d)   |]

derivingUnbox "Variance"
  [t| Variance -> (Int,Double,Double) |]
  [| \(Variance a b c) -> (a,b,c)     |]
  [| \(a,b,c) -> Variance a b c       |]

derivingUnbox "MinD"
  [t| MinD -> Double |]
  [| calcMinD |]
  [| MinD     |]

derivingUnbox "MaxD"
  [t| MaxD -> Double |]
  [| calcMaxD |]
  [| MaxD     |]

derivingUnbox "Weighted"
  [t| forall w a. (Unbox w, Unbox a) => Weighted w a -> (w,a) |]
  [| \(Weighted w a) -> (w,a) |]
  [| \(w,a) -> Weighted w a   |]

derivingUnbox "BinomAcc"
  [t| BinomAcc -> (Int,Int)   |]
  [| \(BinomAcc k n) -> (k,n) |]
  [| \(k,n) -> BinomAcc k n   |]

instance VU.IsoUnbox CountW (Double,Double) where
  toURepr (CountW w w2) = (w,w2)
  fromURepr (w,w2) = CountW w w2
  {-# INLINE toURepr   #-}
  {-# INLINE fromURepr #-}
newtype instance VU.MVector s CountW = MV_CountW (VU.MVector s (Double,Double))
newtype instance VU.Vector    CountW = V_CountW  (VU.Vector    (Double,Double))
deriving via (CountW `VU.As` (Double,Double)) instance VGM.MVector VU.MVector CountW
deriving via (CountW `VU.As` (Double,Double)) instance VG.Vector   VU.Vector  CountW
instance VU.Unbox CountW

-- $references
--
-- * [Welford1962] Welford, B.P. (1962) Note on a method for
--   calculating corrected sums of squares and
--   products. /Technometrics/
--   4(3):419-420. <http://www.jstor.org/stable/1266577>
--
-- * [Chan1979] Chan, Tony F.; Golub, Gene H.; LeVeque, Randall
--   J. (1979), Updating Formulae and a Pairwise Algorithm for
--   Computing Sample Variances., Technical Report STAN-CS-79-773,
--   Department of Computer Science, Stanford University. Page 4.
