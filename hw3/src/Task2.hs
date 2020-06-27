module Task2
  ( ConcurrentHashTable(..)
  , getTable
  , newTable
  , putTable
  , sizeTable
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad (when)
import Data.Foldable (forM_)
import Data.Hashable (Hashable(..))
import qualified Data.Map as M
import qualified Data.Vector as V

-- | Type alias for the concurrent hash table.
type TableMap k v = TVar (M.Map k v)

-- | Concurrent Hash Table : k - key type, v - value type.
data ConcurrentHashTable k v =
  ConcurrentHashTable
    { size :: TVar Int -- ^ size of table wrapped in TVar
    , elements :: TVar (V.Vector (TableMap k v)) -- ^ Vector of maps with elements wrapper in TVar
    }

-- |  Hash table to List of pairs [(key, value)]
toList :: ConcurrentHashTable k v -> STM [(k, v)]
toList table = do
  oldVec <- readTVar $ elements table
  maps <- mapM readTVar $ V.toList oldVec
  return $ concatMap M.toList maps

-- | Adding element to the one particular map
addElement ::
     (Hashable k, Ord k)
  => k -- ^ key
  -> v -- ^ value
  -> ConcurrentHashTable k v -- ^ hash table
  -> STM () -- ^ STM wrapper
addElement key value table = do
  oldElems <- readTVar (elements table) >>= (\v -> return $ v V.! (hash key `mod` V.length v))
  oldMap <- readTVar oldElems
  writeTVar oldElems (M.insert key value oldMap)

-- | Create table of size 100
newTable :: IO (ConcurrentHashTable k v)
newTable =
  atomically $ do
    newSize <- newTVar 0
    emptyElements <- V.replicateM 100 (newTVar M.empty)
    boxedElems <- newTVar emptyElements
    return $ ConcurrentHashTable newSize boxedElems

-- | Get element from table concurrently.
getTable ::
     (Hashable k, Ord k)
  => ConcurrentHashTable k v -- ^ hash table
  -> k -- ^ key
  -> IO (Maybe v) -- ^ Maybe value wrapped in IO
getTable table key =
  atomically
    (M.lookup key <$> (readTVar (elements table) >>= (\v -> return $ v V.! (hash key `mod` V.length v)) >>= readTVar))

-- | Increase the size of hash table when (2 * amount of elements) > current size of container for the good amortized time.
expandTable ::
     (Hashable k, Ord k)
  => ConcurrentHashTable k v -- ^ hash table
  -> Int -- ^ current size of container
  -> STM () -- ^ STM wrapper with expanded container
expandTable table elemSize = do
  entry <- toList table
  newVec <- V.replicateM (2 * elemSize) (newTVar M.empty)
  writeTVar (elements table) newVec
  forM_ entry (\(key', value') -> addElement key' value' table)

-- | Put element in hash table concurrently.
putTable ::
     (Hashable k, Ord k)
  => k -- ^ key
  -> v -- ^ value
  -> ConcurrentHashTable k v -- ^ table
  -> IO ()
putTable key value table =
  atomically $ do
    oldElements <- readTVar $ elements table
    let elemSize = V.length oldElements
    oldSize <- readTVar $ size table
    when (oldSize * 2 > elemSize) $ expandTable table elemSize
    addElement key value table
    modifyTVar (size table) (+ 1)

-- | Returns the size of table concurrently.
sizeTable :: ConcurrentHashTable k v -> IO Int
sizeTable table = atomically $ readTVar (size table)
