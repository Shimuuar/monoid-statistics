
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
  defaultMain [ bench "mean-list-D" $ nf  (calcMean . evalStatistic) sampleD 
              , bench "mean-list-D" $ nf  (calcMean . evalStatistic) sampleI
              , bench "var-list-D"  $ nf  (calcVariance . evalStatistic) sampleI
              , bench "var-list-D"  $ nf  (calcVariance . evalStatistic) sampleI
              ]
