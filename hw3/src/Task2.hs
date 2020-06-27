module Task2
  ( ConcurrentHashTable(..)
  , getTable
  , newTable
  , putTable
  , sizeTable
  ) where

import           Control.Concurrent.STM (STM, TVar, atomically, modifyTVar,
                                         newTVar, readTVar, writeTVar)
import           Control.Monad          (when)
import           Data.Foldable          (forM_)
import           Data.Hashable          (Hashable (..))
import qualified Data.Map               as M
import qualified Data.Vector            as V

type TableMap k v = TVar (M.Map k v)

data ConcurrentHashTable k v =
  ConcurrentHashTable
    { size     :: TVar Int
    , elements :: TVar (V.Vector (TableMap k v))
    }

toList :: ConcurrentHashTable k v -> STM [(k, v)]
toList h = do
  vec <- readTVar $ elements h
  maps <- mapM readTVar $ V.toList vec
  return $ concatMap M.toList maps

addElement :: (Hashable k, Ord k) => k -> v -> ConcurrentHashTable k v -> STM ()
addElement key value table = do
  oldElems <- readTVar (elements table) >>= (\v -> return $ v V.! (hash key `mod` V.length v))
  oldMap <- readTVar oldElems
  writeTVar oldElems (M.insert key value oldMap)

newTable :: IO (ConcurrentHashTable k v)
newTable =
  atomically $ do
    newSize <- newTVar 0
    emptyElements <- V.replicateM 100 (newTVar M.empty)
    boxedElems <- newTVar emptyElements
    return $ ConcurrentHashTable newSize $! boxedElems

getTable :: (Hashable k, Ord k) => ConcurrentHashTable k v -> k -> IO (Maybe v)
getTable table key =
  atomically
    (M.lookup key <$> (readTVar (elements table) >>= (\v -> return $ v V.! (hash key `mod` V.length v)) >>= readTVar))

expandElements :: (Hashable k, Ord k) => ConcurrentHashTable k v -> Int -> STM ()
expandElements table elemSize = do
  entry <- toList table
  newVec <- V.replicateM (2 * elemSize) (newTVar M.empty)
  writeTVar (elements table) newVec
  forM_ entry (\(key', value') -> addElement key' value' table)

putTable :: (Hashable k, Ord k) => k -> v -> ConcurrentHashTable k v -> IO ()
putTable key value table =
  atomically $ do
    oldElements <- readTVar $ elements table
    let elemSize = V.length oldElements
    oldSize <- readTVar $ size table
    when (oldSize * 2 > elemSize) $ expandElements table elemSize
    addElement key value table
    modifyTVar (size table) (+ 1)

sizeTable :: ConcurrentHashTable k v -> IO Int
sizeTable table = atomically $ readTVar (size table)
