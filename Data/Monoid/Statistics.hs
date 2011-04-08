{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
-- |
-- Module     : Data.Monoid.Statistics
-- Copyright  : Copyright (c) 2010, Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
-- 
module Data.Monoid.Statistics ( StatMonoid(..)
                              , evalStatistic
                                -- * Statistic monoids
                              , TwoStats(..)
                                -- * Additional information
                                -- $info
                                -- * Examples
                                -- $examples
                              ) where


import Data.Monoid
import Data.Typeable (Typeable)
import qualified Data.Foldable as F



-- | Monoid which corresponds to some stattics. In order to do so it
--   must be commutative. In many cases it's not practical to
--   construct monoids for each element so 'papennd' was added.
--   First parameter of type class is monoidal accumulator. Second is
--   type of element over which statistic is calculated. 
--
--   Statistic could be calculated with fold over sample. Since
--   accumulator is 'Monoid' such fold could be easily parralelized.
--   Check examples section for more information.
--
--   Instance must satisfy following law:
--
--   > pappend x (pappend y mempty) == pappend x mempty `mappend` pappend y mempty
--   > mappend x y == mappend y x
--
--   It is very similar to Reducer type class from monoids package but
--   require commutative monoids
class Monoid m => StatMonoid m a where
  -- | Add one element to monoid accumulator. P stands for point in
  --   analogy for Pointed.
  pappend :: a -> m -> m

-- | Calculate statistic over 'Foldable'. It's implemented in terms of
--   foldl'.
evalStatistic :: (F.Foldable d, StatMonoid m a) => d a -> m
evalStatistic = F.foldl' (flip pappend) mempty
  

----------------------------------------------------------------
-- Generic monoids
----------------------------------------------------------------

-- | Monoid which allows to calculate two statistics in parralel
data TwoStats a b = TwoStats { calcStat1 :: !a
                             , calcStat2 :: !b
                             }
                    deriving (Show,Eq,Typeable)

instance (Monoid a, Monoid b) => Monoid (TwoStats a b) where
  mempty = TwoStats mempty mempty
  mappend !(TwoStats x y) !(TwoStats x' y') = 
    TwoStats (mappend x x') (mappend y y')
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

instance (StatMonoid a x, StatMonoid b x) => StatMonoid (TwoStats a b) x where
  pappend !x !(TwoStats a b) = TwoStats (pappend x a) (pappend x b)
  {-# INLINE pappend #-}

            
-- $info
--
-- Statistic is function of a sample which does not depend on order of
-- elements in a sample. For each statistics corresponding monoid
-- could be constructed:
--
-- > f :: [A] -> B
-- >
-- > data F = F [A]
-- >
-- > evalF (F xs) = f xs
-- >
-- > instance Monoid F here
-- >   mempty = F []
-- >   (F a) `mappend` (F b) = F (a ++ b)
--
-- This indeed proves that monoid could be constructed. Monoid above
-- is completely impractical. It runs in O(n) space. However for some
-- statistics monoids which runs in O(1) space could be
-- implemented. Simple examples of such statistics are number of
-- elements in sample or mean of a sample.
--
-- On the other hand some statistics could not be implemented in such
-- way. For example calculation of median require O(n) space. Variance
-- could be implemented in O(1) but such implementation will have
-- problems with numberical stability.



-- $examples
--
-- These examples show how to find maximum and minimum of a sample in
-- one pass over data.
-- 
-- This is test data. It's not limited to list but could be anything
-- what could be folded.
--
-- > > let xs = [1..100] :: [Double]
-- 
-- Now let calculate maximum of test sample using two methods. First
-- one is to use generic function 'evalStatistic' and another one is
-- fold.
--
-- > > evalStatistic xs :: Max
-- > Max {calcMax = 100.0}
-- > > foldl (flip pappend) mempty xs :: Max
-- > Max {calcMax = 100.0}
--
-- More complicated example allows to combine several monoids
-- together. It allows to calculate two statistics in one pass:
--
-- > > evalStatistic xs :: TwoStats Min Max
-- > TwoStats {calcStat1 = Min {calcMin = 1.0}, calcStat2 = Max {calcMax = 100.0}}
--
-- Last example shows how to calculate nuber of elements, mean and
-- variance at once:
--
-- > > let v = evalStatistic xs :: Variance
-- > > calcCount v
-- > 100
-- > > calcMean v
-- > 50.5
-- > > calcStddev v
-- > 28.86607004772212
