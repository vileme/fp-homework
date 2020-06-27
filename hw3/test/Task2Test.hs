module Task2Test where

import           Control.Concurrent       (ThreadId, forkIO)
import           Control.Concurrent.Async (concurrently_)
import           Data.Foldable            (for_)
import           System.Random            (randomRIO)

import           Task2                    (ConcurrentHashTable, getTable,
                                           newTable, putTable, sizeTable)

hTableTest :: IO ()
hTableTest = do
  table <- newTable :: IO (ConcurrentHashTable Int Int)
  sizeTableTest table
  let threads = [1 .. 1000]
  let operations = [1 .. 1000]
  concurrently_ (for_ threads $ \i -> forkPut i table operations) (for_ threads $ \i -> forkGet i table operations)
  sizeTableTest table
  print "HashTableTest succeded"

forkPut :: Int -> ConcurrentHashTable Int Int -> [Int] -> IO ThreadId
forkPut thread table operations = do
  key <- randomRIO (0, 1000 :: Int)
  value <- randomRIO (0, 1000 :: Int)
  forkIO $ for_ operations (\_ -> putTableTest thread key value table)

forkGet :: Int -> ConcurrentHashTable Int Int -> [Int] -> IO ThreadId
forkGet thread table operations = do
  key <- randomRIO (0, 1000 :: Int)
  forkIO $ for_ operations (\_ -> getTableTest thread key table)
              

sizeTableTest :: ConcurrentHashTable Int Int -> IO()
sizeTableTest  table = do a <- sizeTable table
                          print $ "SIZE : " ++ show a
getTableTest :: Int -> Int -> ConcurrentHashTable Int Int -> IO ()
getTableTest thread key table = do
  result <- getTable table key
  print $ "Thread : " ++ show thread ++ "     operation ***GET*** with " ++ "key : " ++ show key ++ "   result : " ++ show result

putTableTest :: Int -> Int -> Int -> ConcurrentHashTable Int Int -> IO ()
putTableTest thread key value table = do
  putTable key value table
  sizeTableTest  table
  print $ "Thread : " ++ show thread ++ "     operation ***PUT*** with " ++ "key : " ++ show key ++ "   value : " ++ show value
