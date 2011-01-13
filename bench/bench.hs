
import Criterion.Main

import Control.Monad
import Control.Monad.ST (runST)
import System.Random.MWC

import Data.Monoid.Statistics
import Data.Monoid.Statistics.Numeric

sampleD :: [Double]
sampleD = runST $ replicateM 1000 . uniform =<< create

sampleI :: [Int]
sampleI = runST $ replicateM 1000 . uniform =<< create

main :: IO ()
main = do
  defaultMain [ bench "mean-list-D" $ nf  (calcMean     . asMean     . evalStatistic) sampleD 
              , bench "mean-list-I" $ nf  (calcMean     . asMean     . evalStatistic) sampleI
              , bench "var-list-D"  $ nf  (calcVariance . asVariance . evalStatistic) sampleI
              , bench "var-list-I"  $ nf  (calcVariance . asVariance . evalStatistic) sampleI
              ]
