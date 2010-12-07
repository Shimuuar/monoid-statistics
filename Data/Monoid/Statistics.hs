{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE BangPatterns          #-}
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
                              , Count(..)
                              , TwoStats(..)
                                -- * Additional information
                                -- $info
                              ) where


import Data.Monoid
import qualified Data.Foldable as F



-- | Monoid which corresponds to some stattics. In order to do so it
--   must be commutative. In many cases it's not practical to
--   construct monoids for each element so 'papennd' was added.
--   First parameter of type class is monoidal accumulator. Second is
--   type of element over which statistic is calculated. 
--
--   Statistic could be calculated with fold over sample. Since
--   accumulator is 'Monoid' such fold could be easily parralelized.
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
-- Simple monoids
----------------------------------------------------------------

-- | Simplest statistics. Number of elements in the sample
newtype Count a = Count { calcCount :: a }
                  deriving Show

instance Integral a => Monoid (Count a) where
  mempty = Count 0
  (Count i) `mappend` (Count j) = Count (i + j)
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}
  
instance (Integral a) => StatMonoid (Count a) b where
  pappend _ !(Count n) = Count (n + 1)
  {-# INLINE pappend #-}
  
  
  


----------------------------------------------------------------
-- Generic monoids
----------------------------------------------------------------

-- | Monoid which allows to calculate two statistics in parralel
data TwoStats a b = TwoStats { calcStat1 :: !a
                             , calcStat2 :: !b
                             }

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
-- implemented. For example mean. 
--
-- On the other hand some statistics could not be implemented in such
-- way. For example calculation of median require O(n) space. Variance
-- could be implemented in O(1) but such implementation won't be
-- numerically stable. 
