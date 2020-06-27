module Task2Test where

import           Control.Concurrent       (ThreadId, forkIO)
import           Control.Concurrent.Async (concurrently_)
import           Data.Foldable            (for_)
import           System.Random            (randomRIO)

import           Task2                    (ConcurrentHashTable, getTable,
                                           newTable, putTable)

hTableTest :: IO ()
hTableTest = do
  h <- newTable :: IO (ConcurrentHashTable Int Int)
  let threads = [1 .. 10000]
  let operations = [1 .. 10]
  concurrently_ (for_ threads $ \i -> forkPut i h operations) (for_ threads $ \i -> forkGet i h operations)
  print "HashTableTest succeded"

forkPut :: Int -> ConcurrentHashTable Int Int -> [Int] -> IO ThreadId
forkPut thread h operations = do
  key <- randomRIO (0, 10 :: Int)
  value <- randomRIO (0, 10 :: Int)
  forkIO $ for_ operations (\_ -> putTableTest thread key value h)

forkGet :: Int -> ConcurrentHashTable Int Int -> [Int] -> IO ThreadId
forkGet thread h operations = do
  key <- randomRIO (0, 10 :: Int)
  forkIO $ for_ operations (\_ -> getTableTest thread key h)

getTableTest :: Int -> Int -> ConcurrentHashTable Int Int -> IO ()
getTableTest thread key table = do
  result <- getTable table key
  print $ "Thread : " ++ show thread ++ "     operation ***GET*** with " ++ "key : " ++ show key ++ "   result : " ++ show result

putTableTest :: Int -> Int -> Int -> ConcurrentHashTable Int Int -> IO ()
putTableTest thread key value table = do
  putTable key value table
  print $ "Thread : " ++ show thread ++ "     operation ***PUT*** with " ++ "key : " ++ show key ++ "   value : " ++ show value
