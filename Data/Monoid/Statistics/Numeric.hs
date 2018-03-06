{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Data.Monoid.Statistics.Numeric (
    -- * Mean & Variance
    -- ** Number of elements
    CountG(..)
  , Count
  , asCount
    -- ** Mean
  , MeanKBN(..)
  , asMeanKBN
  , WelfordMean(..)
  , asWelfordMean
  , MeanKahan(..)
  , asMeanKahan
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
import Data.Monoid.Statistics.Class
import Data.Data                    (Typeable,Data)
import Data.Vector.Unboxed          (Unbox)
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Numeric.Sum
import GHC.Generics                 (Generic)
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

-- | Incremental calculation of mean. Sum of elements is calculated
--   using compensated Kahan summation.
data MeanKahan = MeanKahan !Int !KahanSum
             deriving (Show,Eq,Typeable,Data,Generic)

asMeanKahan :: MeanKahan -> MeanKahan
asMeanKahan = id


instance Semigroup MeanKahan where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid MeanKahan where
  mempty = MeanKahan 0 mempty
  MeanKahan 0  _  `mappend` m               = m
  m               `mappend` MeanKahan 0  _  = m
  MeanKahan n1 s1 `mappend` MeanKahan n2 s2 = MeanKahan (n1+n2) (s1 `mappend` s2)

instance Real a => StatMonoid MeanKahan a where
  addValue (MeanKahan n m) x = MeanKahan (n+1) (addValue m x)

instance CalcCount MeanKahan where
  calcCount (MeanKahan n _) = n
instance CalcMean MeanKahan where
  calcMean (MeanKahan 0 _) = Nothing
  calcMean (MeanKahan n s) = Just (kahan s / fromIntegral n)



-- | Incremental calculation of mean. Sum of elements is calculated
--   using Kahan-Babuška-Neumaier summation.
data MeanKBN = MeanKBN !Int !KBNSum
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

instance CalcCount MeanKBN where
  calcCount (MeanKBN n _) = n
instance CalcMean MeanKBN where
  calcMean (MeanKBN 0 _) = Nothing
  calcMean (MeanKBN n s) = Just (kbn s / fromIntegral n)



-- | Incremental calculation of mean. One of algorithm's advantage is
--   protection against double overflow:
--
--   > λ> calcMean $ asMeanKBN     $ reduceSample (replicate 100 1e308)
--   > Just NaN
--   > λ> calcMean $ asWelfordMean $ reduceSample (replicate 100 1e308)
--   > Just 1.0e308
--
--   Algorithm is due to Welford [Welford1962]
data WelfordMean = WelfordMean !Int    -- Number of entries
                               !Double -- Current mean
  deriving (Show,Eq,Typeable,Data,Generic)

-- | Type restricted 'id'
asWelfordMean :: WelfordMean -> WelfordMean
asWelfordMean = id

instance Semigroup WelfordMean where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid WelfordMean where
  mempty = WelfordMean 0 0
  mappend (WelfordMean 0 _) m = m
  mappend m (WelfordMean 0 _) = m
  mappend (WelfordMean n x) (WelfordMean k y)
    = WelfordMean (n + k) ((x*n' + y*k') / (n' + k'))
    where
      n' = fromIntegral n
      k' = fromIntegral k
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

-- | \[ s_n = s_{n-1} + \frac{x_n - s_{n-1}}{n} \]
instance Real a => StatMonoid WelfordMean a where
  addValue (WelfordMean n m) !x
    = WelfordMean n' (m + (realToFrac x - m) / fromIntegral n')
    where
      n' = n+1
  {-# INLINE addValue #-}

instance CalcCount WelfordMean where
  calcCount (WelfordMean n _) = n
instance CalcMean WelfordMean where
  calcMean (WelfordMean 0 _) = Nothing
  calcMean (WelfordMean _ m) = Just m



----------------------------------------------------------------

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
  Max (Just a) `mappend` Max (Just b) = Max (Just $! min a b)
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
--   \[ \sigma = \sqrt{\frac{1}{N}\sum_{i=1}^N(x_i - \bar{x})^2 } \]
calcStddevML :: CalcVariance m => m -> Maybe Double
calcStddevML = fmap sqrt . calcVarianceML



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

derivingUnbox "WelfordMean"
  [t| WelfordMean -> (Int,Double) |]
  [| \(WelfordMean a b) -> (a,b)  |]
  [| \(a,b) -> WelfordMean a b    |]

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
