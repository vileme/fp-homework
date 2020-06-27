--import           Criterion.Main
--import           Data.List      (unfoldr)
--import           System.Random  (StdGen, newStdGen, random, randomR, randoms, randomRIO)
import           System.Random  (randomRIO)
--import           Task1
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Concurrent.Async(concurrently)

import Data.Hashable (Hashable(..))
import Data.Foldable (for_)

import Task4 (ConcurrentHashTable, getCHT, newCHT, putCHT)


--
--randomlist :: Int -> StdGen -> [Int]
--randomlist n = take n . unfoldr (Just . random)
--
--randomFigure :: Int -> StdGen -> StdGen -> [Point]
--randomFigure n g g2 = zipWith Point l1 l2
--  where
--    l1 = randomlist n g
--    l2 = randomlist n g2

main :: IO ()
main = do
--    seed <- newStdGen
--  seed2 <- newStdGen
--  let figure = randomFigure 10000000 seed seed2
--  defaultMain
--    [ bgroup "perimeterFast" [bench "fast" $ whnf perimeter figure]
--    , bgroup "perimeterSlow" [bench "slow" $ whnf perimeterSlow figure]
--    , bgroup "doubleAreaFast" [bench "fast" $ whnf doubleArea figure]
--    , bgroup "doubleAreaSlow" [bench "slow" $ whnf doubleAreaSlow figure]
--    ]
    h <- newCHT :: IO (ConcurrentHashTable Int Int)
    let threads = [1..1000]
    let operations = [1..100]
    concurrentlyfor_ threads $ \i -> forkPut i h operations
    for_ threads $ \i -> forkGet i h operations
    threadDelay 2000000
    putStrLn "Main thread finishes"

forkPut :: Int -> ConcurrentHashTable Int Int -> [Int] -> IO ThreadId
forkPut thread h  operations = do
  key <- randomRIO (0,100 :: Int)
  value <- randomRIO (0,100 :: Int)
  forkIO $ for_ operations (\_ -> putCHTTest thread key value h)
forkGet :: Int -> ConcurrentHashTable Int Int -> [Int] -> IO ThreadId
forkGet thread h operations  = do
  key <- randomRIO (0,100 :: Int)
  forkIO $ for_ operations (\_ -> getCHTTest thread key h)


getCHTTest
  :: (Hashable k, Ord k, Show k, Show v)
  => Int
  -> k
  -> ConcurrentHashTable k v
  -> IO ()
getCHTTest thread k h = do
  val <- getCHT k h
  print $ show thread ++ " |GET| " ++ "k: " ++ show k ++ ", v: " ++ show val

putCHTTest
  :: (Hashable k, Ord k, Show k, Show v)
  => Int
  -> k
  -> v
  -> ConcurrentHashTable k v
  -> IO ()
putCHTTest thread k v h = do
  putCHT k v h
  print $ show thread ++ " |PUT| " ++ "k: " ++ show k ++ ", v: " ++ show v

