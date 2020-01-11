{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
-- |
-- Monoids for calculating various statistics in constant space
module Statistics.Monoid.Numeric (
    -- * Mean & Variance
    -- ** Number of elements
    CountG(..)
  , Count
  , asCount
    -- ** Mean
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
  , WMeanKBN
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
    -- * Accessors
  , CalcCount(..)
  , CalcMean(..)
  , CalcVariance(..)
  , calcStddev
  , calcStddevML
    -- * References
    -- $references
  ) where

import Data.Semigroup               (Semigroup(..))
import Data.Monoid                  (Monoid(..))
import Data.Data                    (Typeable,Data)
import Data.Vector.Unboxed          (Unbox)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Numeric.Sum
import GHC.Generics                 (Generic)

import Statistics.Monoid.Class
-- COMPAT
import qualified Data.Vector.Generic         -- Needed for GHC7.4
import qualified Data.Vector.Generic.Mutable -- Needed for GHC7.4


----------------------------------------------------------------
-- Statistical monoids
----------------------------------------------------------------

-- | Calculate number of elements in the sample.
newtype CountG a = CountG { calcCountN :: a }
                  deriving (Show,Eq,Ord,Typeable)

type Count = CountG Int

-- | Type restricted 'id'
asCount :: CountG a -> CountG a
asCount = id

instance Integral a => Semigroup (CountG a) where
  (<>) = mappend
  {-# INLINE (<>) #-}

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
             deriving (Show,Eq,Typeable,Data,Generic)

asMeanNaive :: MeanNaive -> MeanNaive
asMeanNaive = id


instance Semigroup MeanNaive where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid MeanNaive where
  mempty = MeanNaive 0 0
  MeanNaive 0  _  `mappend` m               = m
  m               `mappend` MeanNaive 0  _  = m
  MeanNaive n1 s1 `mappend` MeanNaive n2 s2 = MeanNaive (n1+n2) (s1 + s2)

instance Real a => StatMonoid MeanNaive a where
  addValue (MeanNaive n m) x = MeanNaive (n+1) (m + realToFrac x)
  {-# INLINE addValue #-}

instance CalcCount MeanNaive where
  calcCount (MeanNaive n _) = n
instance CalcMean MeanNaive where
  calcMean (MeanNaive 0 _) = Nothing
  calcMean (MeanNaive n s) = Just (s / fromIntegral n)


----------------------------------------------------------------

-- | Incremental calculation of mean. It tracks separately number of
--   elements and running sum. It uses algorithm for compensated
--   summation which works with mantissa of double size at cost of
--   doing more operations. This means that it's usually possible to
--   compute sum (and therefore mean) within 1 ulp.
data MeanKBN = MeanKBN !Int {-# UNPACK #-} !KBNSum
             deriving (Show,Eq,Typeable,Data,Generic)

asMeanKBN :: MeanKBN -> MeanKBN
asMeanKBN = id


instance Semigroup MeanKBN where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid MeanKBN where
  mempty = MeanKBN 0 mempty
  MeanKBN 0  _  `mappend` m             = m
  m             `mappend` MeanKBN 0  _  = m
  MeanKBN n1 s1 `mappend` MeanKBN n2 s2 = MeanKBN (n1+n2) (s1 `mappend` s2)

instance Real a => StatMonoid MeanKBN a where
  addValue (MeanKBN n m) x = MeanKBN (n+1) (addValue m x)
  {-# INLINE addValue #-}

instance CalcCount MeanKBN where
  calcCount (MeanKBN n _) = n
instance CalcMean MeanKBN where
  calcMean (MeanKBN 0 _) = Nothing
  calcMean (MeanKBN n s) = Just (kbn s / fromIntegral n)


----------------------------------------------------------------

-- | Incremental calculation of weighed mean. Sum of both weights and
--   elements is calculated using Kahan-Babuška-Neumaier summation.
data WMeanKBN = WMeanKBN {-# UNPACK #-} !KBNSum {-# UNPACK #-} !KBNSum
              deriving (Show,Eq,Typeable,Data,Generic)

asWMeanKBN :: WMeanKBN -> WMeanKBN
asWMeanKBN = id


instance Semigroup WMeanKBN where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid WMeanKBN where
  mempty = WMeanKBN mempty mempty
  WMeanKBN n1 s1 `mappend` WMeanKBN n2 s2 = WMeanKBN (n1 <> n2) (s1 <> s2)

instance (Real w, Real a) => StatMonoid WMeanKBN (Weighted w a) where
  addValue (WMeanKBN n m) (Weighted w a)
    = WMeanKBN (add n w') (add m (w' * a'))
    where
      w' = realToFrac w :: Double
      a' = realToFrac a :: Double
  {-# INLINE addValue #-}

instance CalcMean WMeanKBN where
  calcMean (WMeanKBN (kbn -> w) (kbn -> s))
    | w <= 0    = Nothing
    | otherwise = Just (s / w)


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

  
-- | Incremental algorithms for calculation the standard deviation.
data Variance = Variance {-# UNPACK #-} !Int    --  Number of elements in the sample
                         {-# UNPACK #-} !Double -- Current sum of elements of sample
                         {-# UNPACK #-} !Double -- Current sum of squares of deviations from current mean
                deriving (Show,Eq,Typeable)

-- | Type restricted 'id '
asVariance :: Variance -> Variance
asVariance = id
{-# INLINE asVariance #-}

instance Semigroup Variance where
  (<>) = mappend
  {-# INLINE (<>) #-}

-- | Iterative algorithm for calculation of variance [Chan1979]
instance Monoid Variance where
  mempty = Variance 0 0 0
  mappend (Variance n1 ta sa) (Variance n2 tb sb)
    = Variance (n1+n2) (ta+tb) sumsq
    where
      na = fromIntegral n1
      nb = fromIntegral n2
      nom = sqr (ta * nb - tb * na)
      sumsq | n1 == 0   = sb
            | n2 == 0   = sa
            | otherwise = sa + sb + nom / ((na + nb) * na * nb)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

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
              deriving (Show,Eq,Ord,Typeable,Data,Generic)

instance Ord a => Semigroup (Min a) where
  (<>) = mappend
  {-# INLINE (<>) #-}

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
              deriving (Show,Eq,Ord,Typeable,Data,Generic)

instance Ord a => Semigroup (Max a) where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  Max (Just a) `mappend` Max (Just b) = Max (Just $! max a b)
  Max a        `mappend` Max Nothing  = Max a
  Max Nothing  `mappend` Max b        = Max b

instance (Ord a, a ~ a') => StatMonoid (Max a) a' where
  singletonMonoid a = Max (Just a)


----------------------------------------------------------------

-- | Calculate minimum of sample of Doubles. For empty sample returns NaN. Any
--   NaN encountered will be ignored.
newtype MinD = MinD { calcMinD :: Double }
              deriving (Show,Typeable,Data,Generic)

instance Eq MinD where
  MinD a == MinD b
    | isNaN a && isNaN b = True
    | otherwise          = a == b

instance Semigroup MinD where
  (<>) = mappend
  {-# INLINE (<>) #-}

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
              deriving (Show,Typeable,Data,Generic)

instance Eq MaxD where
  MaxD a == MaxD b
    | isNaN a && isNaN b = True
    | otherwise          = a == b

instance Semigroup MaxD where
  (<>) = mappend
  {-# INLINE (<>) #-}

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

-- | Accumulator for binomial trials.
data BinomAcc = BinomAcc { binomAccSuccess :: !Int
                         , binomAccTotal   :: !Int
                         }
  deriving (Show,Eq,Ord,Typeable,Data,Generic)

-- | Type restricted 'id'
asBinomAcc :: BinomAcc -> BinomAcc
asBinomAcc = id

instance Semigroup BinomAcc where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid BinomAcc where
  mempty = BinomAcc 0 0
  mappend (BinomAcc n1 m1) (BinomAcc n2 m2) = BinomAcc (n1+n2) (m1+m2)

instance StatMonoid BinomAcc Bool where
  addValue (BinomAcc nS nT) True  = BinomAcc (nS+1) (nT+1)
  addValue (BinomAcc nS nT) False = BinomAcc  nS    (nT+1)



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

derivingUnbox "MeanKBN"
  [t| MeanKBN -> (Int,Double,Double) |]
  [| \(MeanKBN a (KBNSum b c)) -> (a,b,c)   |]
  [| \(a,b,c) -> MeanKBN a (KBNSum b c) |]

derivingUnbox "Variance"
  [t| Variance -> (Int,Double,Double) |]
  [| \(Variance a b c) -> (a,b,c)  |]
  [| \(a,b,c) -> Variance a b c    |]

derivingUnbox "MinD"
  [t| MinD -> Double |]
  [| calcMinD |]
  [| MinD     |]

derivingUnbox "MaxD"
  [t| MaxD -> Double |]
  [| calcMaxD |]
  [| MaxD     |]

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
