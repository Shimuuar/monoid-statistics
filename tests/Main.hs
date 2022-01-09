{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
--
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where
import Data.Typeable
import Numeric.Sum
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure (ignoreTest)

import Data.Monoid.Statistics
import Data.Monoid.Statistics.Extra


----------------------------------------------------------------
-- Properties
----------------------------------------------------------------

class MonoidProperty m where
  isAssociative, isCommutative, isMemptyDistribute, isMemptyNeutral :: Bool
  isMemptyNeutral    = True
  isMemptyDistribute = True
  isAssociative      = True
  isCommutative      = True

instance {-# OVERLAPPABLE #-} MonoidProperty m
instance MonoidProperty MeanNaive where
  isAssociative = False
instance MonoidProperty WelfordMean where
  isAssociative      = False
  isMemptyDistribute = False
instance MonoidProperty MeanKahan where
  isAssociative      = False
  isCommutative      = False
  isMemptyDistribute = False
instance MonoidProperty MeanKBN where
  isAssociative = False
  isCommutative = False
instance MonoidProperty Variance where
  isAssociative = False
instance MonoidProperty WMeanNaive where
  isAssociative   = False
instance MonoidProperty WMeanKBN where
  isMemptyNeutral = False
  isAssociative   = False
  isCommutative   = False

p_memptyIsNeutral
  :: forall m. (Monoid m, MonoidProperty m, Arbitrary m, Show m, Eq m)
  => TestTree
p_memptyIsNeutral
  = (if isMemptyNeutral @m then id else ignoreTest)
  $ testProperty "mempty is neutral" $ \(m :: m) ->
    counterexample ("m <> mempty = " ++ show (m <> mempty))
  $ counterexample ("mempty <> m = " ++ show (mempty <> m))
  $  (m <> mempty) == m
  && (mempty <> m) == m

p_associativity
  :: forall m. (MonoidProperty m, Monoid m, Arbitrary m, Show m, Eq m)
  => TestTree
p_associativity
  = (if isAssociative @m then id else ignoreTest)
  $ testProperty "associativity" $ \(a :: m) b c ->
    let val1 = (a <> b) <> c
        val2 = a <> (b <> c)
    in counterexample ("left : " ++ show val1)
     $ counterexample ("right: " ++ show val2)
     $ val1 == val2

p_commutativity
  :: forall m. (Monoid m, MonoidProperty m, Arbitrary m, Show m, Eq m)
  => TestTree
p_commutativity
  = (if isCommutative @m then id else ignoreTest)
  $ testProperty "commutativity" $ \(a :: m) b ->
    let val1 = a <> b
        val2 = b <> a
    in counterexample ("a <> b = " ++ show val1)
     $ counterexample ("b <> a = " ++ show val2)
     $ val1 == val2

p_addValue1
  :: forall m a. ( StatMonoid m a
                 , Eq m
                 , Arbitrary a, Show a)
  => TestTree
p_addValue1
  = testProperty "addValue x mempty == singletonMonoid" $ \(a :: a) ->
    singletonMonoid a == addValue (mempty :: m) a


p_addValue2
  :: forall m a. ( MonoidProperty m, StatMonoid m a
                 , Show m, Eq m
                 , Arbitrary a, Show a)
  => TestTree
p_addValue2
  = (if isMemptyDistribute @m then id else ignoreTest)
  $ testProperty "addValue (addValue m x) y = addValue 0 x <> addValue 0 y" $ \(x :: a) (y :: a) ->
    let val1 = addValue (addValue mempty y) x
        val2 = (addValue mempty x <> addValue (mempty :: m) y)
    in counterexample ("left : " ++ show val1)
     $ counterexample ("right: " ++ show val2)
     $ val1 == val2



----------------------------------------------------------------

testMonoid
  :: forall m a.
     ( StatMonoid m a, MonoidProperty m
     , Typeable a, Typeable m, Arbitrary a, Arbitrary m, Show a, Show m, Eq m)
  => [TestTree] -> TestTree
testMonoid tests
  = testGroup (show (typeOf (undefined :: m)) ++ " <= " ++ show (typeOf (undefined :: a)))
  $ [ p_memptyIsNeutral @m
    , p_associativity   @m
    , p_commutativity   @m
    , p_addValue1       @m @a
    , p_addValue2       @m @a
    ]
 ++ tests

testMeanMonoid
  :: forall m.
     ( StatMonoid m Double, CalcMean m, CalcCount m, MonoidProperty m
     , Typeable m, Arbitrary m, Show m, Eq m)
  => [TestTree] -> TestTree
testMeanMonoid tests
  = testMonoid @m @Double
  $ [ testCase "Count" $ do
        let m = reduceSample @m testSample
        testSampleCount     @=? calcCount m
    , testCase "Mean" $ do
        let m = reduceSample @m testSample
        Just testSampleMean @=? calcMean  m
    , testCase "Mean (empty sample)" $ do
        let m = reduceSample @m @Double []
        Nothing @=? calcMean m
    ] ++ tests

testVarianceMonoid
  :: forall m.
     ( StatMonoid m Double, CalcVariance m, CalcMean m, CalcCount m, MonoidProperty m
     , Typeable m, Arbitrary m, Show m, Eq m)
  => [TestTree] -> TestTree
testVarianceMonoid tests
  = testMeanMonoid @m
  $ [ testCase "Variance (unbiased)" $ do
        let m = reduceSample @m testSample
        Just testSampleVariance   @=? calcVariance m
    , testCase "Variance (ML)" $ do
        let m = reduceSample @m testSample
        Just testSampleVarianceML @=? calcVarianceML m
    ] ++ tests

testWMeanMonoid
  :: forall m.
     ( StatMonoid m (Weighted Double Double), CalcMean m, MonoidProperty m
     , Typeable m, Arbitrary m, Show m, Eq m)
  => [TestTree] -> TestTree
testWMeanMonoid tests
  = testMonoid @m @(Weighted Double Double)
  $ [ testCase "Mean" $ do
        let m = reduceSample @m testWSample
        Just testWSampleMean @=? calcMean  m
    ] ++ tests


main :: IO ()
main = defaultMain $ testGroup "monoid-statistics"
  [ testMonoid @(CountG Int) @Int
    [ testCase "CountG"  $ let xs = "acbdef"
                               n  = reduceSample xs :: Count
                           in length xs @=? calcCount n
    ]
  , testMonoid @(Min Int)    @Int
    [ testCase "Min []"  $ let xs = []
                               n  = reduceSample xs :: Min Int
                           in Nothing @=? calcMin n
    , testCase "Min"     $ let xs = [1..10]
                               n  = reduceSample xs :: Min Int
                           in Just (minimum xs) @=? calcMin n
    ]
  , testMonoid @(Max Int)    @Int
    [ testCase "Max []"  $ let xs = []
                               n  = reduceSample xs :: Max Int
                           in Nothing @=? calcMax n
    , testCase "Max"     $ let xs = [1..10]
                               n  = reduceSample xs :: Max Int
                           in Just (maximum xs) @=? calcMax n
    ]
  , testMonoid @MinD         @Double
    [ testCase "MinD"    $ let xs = [1..10]
                               n  = reduceSample xs :: MinD
                           in minimum xs @=? calcMinD n
    ]
  , testMonoid @MaxD         @Double
    [ testCase "MaxD"    $ let xs = [1..10]
                               n  = reduceSample xs :: MaxD
                           in maximum xs @=? calcMaxD n

    ]
  , testMonoid @BinomAcc     @Bool   []
    -- Numeric accumulators
  , testMeanMonoid     @MeanNaive   []
  , testMeanMonoid     @WelfordMean []
  , testMeanMonoid     @MeanKahan   []
  , testMeanMonoid     @MeanKBN     []
  , testWMeanMonoid    @WMeanNaive  []
  , testWMeanMonoid    @WMeanKBN    []
  , testVarianceMonoid @Variance    []
  ]

-- | Test sample for which we could compute statistics exactly, and
--   any reasonable algorithm should be able to return exact answer as
--   well
testSample :: [Double]
testSample = [1..10]

testWSample :: [Weighted Double Double]
testWSample = [Weighted x x | x <- [1..10]]

testSampleCount :: Int
testSampleCount = length testSample

testSampleMean,testWSampleMean :: Double
testSampleMean  = 5.5
testWSampleMean = 7.0

testSampleVariance,testSampleVarianceML :: Double
testSampleVariance   = 9.166666666666666
testSampleVarianceML = 8.25

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

instance Arbitrary MeanNaive where
  arbitrary = arbitrary >>= \x -> case x of
    NonNegative 0 -> return mempty
    NonNegative n -> do m <- arbitrary
                        return (MeanNaive n m)

instance Arbitrary WelfordMean where
  arbitrary = arbitrary >>= \x -> case x of
    NonNegative 0 -> return mempty
    NonNegative n -> do m <- arbitrary
                        return (WelfordMean n m)

instance Arbitrary MeanKahan where
  arbitrary = do
    n <- arbitrary
    s <- arbitraryKBN n
    return $! MeanKahan (getNonNegative n) s

instance Arbitrary MeanKBN where
  arbitrary = do
    n <- arbitrary
    s <- arbitraryKBN n
    return $! MeanKBN (getNonNegative n) s

instance Arbitrary WMeanKBN where
  arbitrary = do
    n            <- arbitrary
    KBNSum w1 w2 <- arbitraryKBN n
    s            <- arbitraryKBN n
    return $! WMeanKBN (KBNSum (abs w1) w2) s

instance Arbitrary WMeanNaive where
  arbitrary = do
    NonNegative w <- arbitrary
    s             <- arbitrary
    return $! WMeanNaive w s

instance Arbitrary Variance where
  arbitrary = arbitrary >>= \x -> case x of
    NonNegative 0 -> return mempty
    NonNegative n -> do
      m             <- arbitrary
      NonNegative s <- arbitrary
      return $ Variance n m s

instance (Arbitrary a, Arbitrary w) => Arbitrary (Weighted w a) where
  arbitrary = Weighted <$> arbitrary <*> arbitrary

arbitraryKBN :: Summation a => NonNegative Int -> Gen a
arbitraryKBN (NonNegative 0) = return zero
arbitraryKBN (NonNegative 1) = do
  x1 <- arbitrary
  return $! zero `add` x1
arbitraryKBN _ = do
  x1 <- arbitrary
  x2 <- arbitrary
  x3 <- arbitrary
  return $! ((zero `add` x1) `add` x2) `add` x3
