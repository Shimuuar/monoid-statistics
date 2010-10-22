{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Data.Monoid.Statistics ( PointedMonoid(..)
                              ) where

import Data.Monoid

-- |
class Monoid m => PointedMonoid m a where
    -- |
    pointMonoid :: a -> m
    -- |
    appendR :: m -> a -> m
    appendR m a = m `mappend` pointMonoid a
    -- |
    appendL :: a -> m -> m
    appendL a m = pointMonoid a `mappend` m


instance PointedMonoid m a => PointedMonoid (Dual m) a where
    pointMonoid = Dual . pointMonoid
    appendR (Dual m) a = Dual $ appendL a m
    appendL a (Dual m) = Dual $ appendR m a
                         
instance PointedMonoid All Bool where
    pointMonoid = All

instance PointedMonoid Any Bool where
    pointMonoid = Any

instance Num a => PointedMonoid (Sum a) a where
    pointMonoid = Sum
    
instance Num a => PointedMonoid (Product a) a where
    pointMonoid = Product
    
instance PointedMonoid (First a) (Maybe a) where
    pointMonoid = First
instance PointedMonoid (First a) a where
    pointMonoid = First . Just
    
instance PointedMonoid (Last a) (Maybe a) where
    pointMonoid = Last
instance PointedMonoid (Last a) a where
    pointMonoid = Last . Just
    