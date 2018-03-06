{-# LANGUAGE ScopedTypeVariables #-}
--
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Data.Monoid
import Data.Typeable
import Numeric.Sum
import Test.Tasty
import Test.Tasty.QuickCheck

import Statistics.Monoid


data T a = T

p_memptyIsNeutral
  :: forall m. (Monoid m, Arbitrary m, Show m, Eq m)
  => T m -> TestTree
p_memptyIsNeutral _
  = testProperty "mempty is neutral" $ \(m :: m) ->
       (m <> mempty) == m
    && (mempty <> m) == m

p_associativity
  :: forall m. (Monoid m, Arbitrary m, Show m, Eq m)
  => T m -> TestTree
p_associativity _
  = testProperty "associativity" $ \(a :: m) b c ->
    let val1 = (a <> b) <> c
        val2 = a <> (b <> c)
    in counterexample ("left : " ++ show val1)
     $ counterexample ("right: " ++ show val2)
     $ val1 == val2

p_commutativity
  :: forall m. (Monoid m, Arbitrary m, Show m, Eq m)
  => T m -> TestTree
p_commutativity _
  = testProperty "commutativity" $ \(a :: m) b ->
    (a <> b) == (b <> a)

p_addValue1
  :: forall a m. ( StatMonoid m a
                 , Arbitrary m, Show m, Eq m
                 , Arbitrary a, Show a, Eq a)
  => T a -> T m -> TestTree
p_addValue1 _ _
  = testProperty "addValue x mempty == singletonMonoid" $ \(a :: a) ->
    singletonMonoid a == addValue (mempty :: m) a


p_addValue2
  :: forall a m. ( StatMonoid m a
                 , Arbitrary m, Show m, Eq m
                 , Arbitrary a, Show a, Eq a)
  => T a -> T m -> TestTree
p_addValue2 _ _
  = testProperty "addValue law" $ \(x :: a) (y :: a) ->
    let val1 = addValue (addValue mempty y) x
        val2 = (addValue mempty x <> addValue (mempty :: m) y)
    in counterexample ("left : " ++ show val1)
     $ counterexample ("right: " ++ show val2)
     $ val1 == val2



----------------------------------------------------------------

testType :: forall m. Typeable m => T m -> [T m -> TestTree] -> TestTree
testType t props = testGroup (show (typeOf (undefined :: m)))
                             (fmap ($ t) props)


main :: IO ()
main = defaultMain $ testGroup "monoid-statistics"
  [ testType (T :: T (CountG Int))
      [ p_memptyIsNeutral
      , p_associativity
      , p_commutativity
      , p_addValue1 (T :: T Int)
      , p_addValue2 (T :: T Int)
      ]
  , testType (T :: T (Min Int))
      [ p_memptyIsNeutral
      , p_associativity
      , p_commutativity
      , p_addValue1 (T :: T Int)
      , p_addValue2 (T :: T Int)
      ]
  , testType (T :: T (Max Int))
      [ p_memptyIsNeutral
      , p_associativity
      , p_commutativity
      , p_addValue1 (T :: T Int)
      , p_addValue2 (T :: T Int)
      ]
  , testType (T :: T MinD)
      [ p_memptyIsNeutral
      , p_associativity
      , p_commutativity
      , p_addValue1 (T :: T Double)
      , p_addValue2 (T :: T Double)
      ]
  , testType (T :: T MaxD)
      [ p_memptyIsNeutral
      , p_associativity
      , p_commutativity
      , p_addValue1 (T :: T Double)
      , p_addValue2 (T :: T Double)
      ]
  , testType (T :: T BinomAcc)
      [ p_memptyIsNeutral
      , p_associativity
      , p_commutativity
      , p_addValue1 (T :: T Bool)
      , p_addValue2 (T :: T Bool)
      ]
  , testType (T :: T WelfordMean)
      [ p_memptyIsNeutral
      -- , p_associativity
      , p_commutativity
      , p_addValue1 (T :: T Double)
      -- , p_addValue2 (T :: T Double)
      ]
  , testType (T :: T MeanKBN)
      [ p_memptyIsNeutral
      -- , p_associativity
      -- , p_commutativity
      , p_addValue1 (T :: T Double)
      , p_addValue2 (T :: T Double)
      ]
  , testType (T :: T MeanKahan)
      [ p_memptyIsNeutral
      -- , p_associativity
      -- , p_commutativity
      , p_addValue1 (T :: T Double)
      -- , p_addValue2 (T :: T Double)
      ]
  , testType (T :: T Variance)
      [ p_memptyIsNeutral
      -- , p_associativity
      , p_commutativity
      , p_addValue1 (T :: T Double)
      , p_addValue2 (T :: T Double)
      ]
  ]

----------------------------------------------------------------

instance (Arbitrary a, Num a, Ord a) => Arbitrary (CountG a) where
  arbitrary = do
    NonNegative n <- arbitrary
    return (CountG n)

instance (Arbitrary a) => Arbitrary (Max a) where
  arbitrary = fmap Max arbitrary

instance (Arbitrary a) => Arbitrary (Min a) where
  arbitrary = fmap Min arbitrary

instance Arbitrary MinD where
  arbitrary = frequency [ (1, return mempty)
                        , (4, fmap MinD arbitrary)
                        ]

instance Arbitrary MaxD where
  arbitrary = frequency [ (1, return mempty)
                        , (4, fmap MaxD arbitrary)
                        ]

instance Arbitrary BinomAcc where
  arbitrary = do
    NonNegative nSucc <- arbitrary
    NonNegative nFail <- arbitrary
    return $ BinomAcc nSucc (nFail + nSucc)

instance Arbitrary WelfordMean where
  arbitrary = arbitrary >>= \x -> case x of
    NonNegative 0 -> return mempty
    NonNegative n -> do m <- arbitrary
                        return (WelfordMean n m)

instance Arbitrary Variance where
  arbitrary = arbitrary >>= \x -> case x of
    NonNegative 0 -> return mempty
    NonNegative n -> do
      m             <- arbitrary
      NonNegative s <- arbitrary
      return $ Variance n m s

instance Arbitrary MeanKBN where
  arbitrary = arbitrary >>= \x -> case x of
    NonNegative 0 -> return mempty
    NonNegative n -> do
      x1 <- arbitrary
      x2 <- arbitrary
      x3 <- arbitrary
      return $ MeanKBN n (((zero `add` x1) `add` x2) `add` x3)

instance Arbitrary MeanKahan where
  arbitrary = arbitrary >>= \x -> case x of
    NonNegative 0 -> return mempty
    NonNegative n -> do
      x1 <- arbitrary
      x2 <- arbitrary
      x3 <- arbitrary
      return $ MeanKahan n (((zero `add` x1) `add` x2) `add` x3)
