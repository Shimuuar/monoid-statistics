-- |
module Main where

import Control.Monad
import Control.Monad.ST (runST)

import qualified Data.Vector.Unboxed as U

import Criterion.Main
import System.Random.MWC
--
import Numeric.Sum
import Statistics.Monoid


sampleD10,sampleD100,sampleD1000 :: U.Vector Double
sampleD10   = runST $ U.replicateM 10   . uniform =<< create
sampleD100  = runST $ U.replicateM 100  . uniform =<< create
sampleD1000 = runST $ U.replicateM 1000 . uniform =<< create

sampleI10,sampleI100,sampleI1000 :: U.Vector Int
sampleI10   = runST $ U.replicateM 10   . uniform =<< create
sampleI100  = runST $ U.replicateM 100  . uniform =<< create
sampleI1000 = runST $ U.replicateM 1000 . uniform =<< create

main :: IO ()
main = defaultMain
  [ bgroup "Count"
      [ bench "10   D" $ whnf (reduceSampleVec :: U.Vector Double -> Count) sampleD10
      , bench "100  D" $ whnf (reduceSampleVec :: U.Vector Double -> Count) sampleD100
      , bench "1000 D" $ whnf (reduceSampleVec :: U.Vector Double -> Count) sampleD1000
      ]
  , bgroup "KBN"
      [ bench "10   D" $ whnf (reduceSampleVec :: U.Vector Double -> KBNSum) sampleD10
      , bench "100  D" $ whnf (reduceSampleVec :: U.Vector Double -> KBNSum) sampleD100
      , bench "1000 D" $ whnf (reduceSampleVec :: U.Vector Double -> KBNSum) sampleD1000
      ]
  , bgroup "Kahan"
      [ bench "10   D" $ whnf (reduceSampleVec :: U.Vector Double -> KahanSum) sampleD10
      , bench "100  D" $ whnf (reduceSampleVec :: U.Vector Double -> KahanSum) sampleD100
      , bench "1000 D" $ whnf (reduceSampleVec :: U.Vector Double -> KahanSum) sampleD1000
      ]
  , bgroup "MeanKBN"
      [ bench "10   D" $ whnf (reduceSampleVec :: U.Vector Double -> MeanKBN) sampleD10
      , bench "100  D" $ whnf (reduceSampleVec :: U.Vector Double -> MeanKBN) sampleD100
      , bench "1000 D" $ whnf (reduceSampleVec :: U.Vector Double -> MeanKBN) sampleD1000
      , bench "10   I" $ whnf (reduceSampleVec :: U.Vector Int    -> MeanKBN) sampleI10
      , bench "100  I" $ whnf (reduceSampleVec :: U.Vector Int    -> MeanKBN) sampleI100
      , bench "1000 I" $ whnf (reduceSampleVec :: U.Vector Int    -> MeanKBN) sampleI1000
      ]
  , bgroup "WelfordMean"
      [ bench "10   D" $ whnf (reduceSampleVec :: U.Vector Double -> WelfordMean) sampleD10
      , bench "100  D" $ whnf (reduceSampleVec :: U.Vector Double -> WelfordMean) sampleD100
      , bench "1000 D" $ whnf (reduceSampleVec :: U.Vector Double -> WelfordMean) sampleD1000
      , bench "10   I" $ whnf (reduceSampleVec :: U.Vector Int    -> WelfordMean) sampleI10
      , bench "100  I" $ whnf (reduceSampleVec :: U.Vector Int    -> WelfordMean) sampleI100
      , bench "1000 I" $ whnf (reduceSampleVec :: U.Vector Int    -> WelfordMean) sampleI1000
      ]
  , bgroup "Variance"
      [ bench "10   D" $ whnf (reduceSampleVec :: U.Vector Double -> Variance) sampleD10
      , bench "100  D" $ whnf (reduceSampleVec :: U.Vector Double -> Variance) sampleD100
      , bench "1000 D" $ whnf (reduceSampleVec :: U.Vector Double -> Variance) sampleD1000
      , bench "10   I" $ whnf (reduceSampleVec :: U.Vector Int    -> Variance) sampleI10
      , bench "100  I" $ whnf (reduceSampleVec :: U.Vector Int    -> Variance) sampleI100
      , bench "1000 I" $ whnf (reduceSampleVec :: U.Vector Int    -> Variance) sampleI1000
      ]
  ]
