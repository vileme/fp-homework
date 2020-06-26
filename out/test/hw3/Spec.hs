import           Criterion.Main
import           Data.List      (unfoldr)
import           System.Random  (StdGen, newStdGen, random)
import           Task1

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

randomFigure :: Int -> StdGen -> StdGen -> [Point]
randomFigure n g g2 = zipWith Point l1 l2
  where
    l1 = randomlist n g
    l2 = randomlist n g2

main :: IO ()
main = do
  seed <- newStdGen
  seed2 <- newStdGen
  let figure = randomFigure 10000000 seed seed2
  defaultMain
    [ bgroup "perimeterFast" [bench "fast" $ whnf perimeter figure]
    , bgroup "perimeterSlow" [bench "slow" $ whnf perimeterSlow figure]
    , bgroup "doubleAreaFast" [bench "fast" $ whnf doubleArea figure]
    , bgroup "doubleAreaSlow" [bench "slow" $ whnf doubleAreaSlow figure]
    ]
