{-# LANGUAGE BangPatterns #-}

module Task4
  ( ConcurrentHashTable (..)
  , getCHT
  , newCHT
  , putCHT
  , sizeCHT
  ) where

import           Control.Concurrent.STM (STM, TVar, atomically, readTVar, newTVar,
                                         modifyTVar', writeTVar)

import           Data.Hashable (Hashable(..))
import           Data.Foldable (forM_)
import qualified Data.Vector as V
import qualified Data.Map as M

data ConcurrentHashTable k v = ConcurrentHashTable
  { count     :: TVar Int
  , getVector :: TVar (V.Vector (TVar (M.Map k v)))
  }

getBucket :: Hashable k => ConcurrentHashTable k v -> k -> STM (TVar (M.Map k v))
getBucket h k = do
  vec <- readTVar $ getVector h
  return $! vec V.! (hash k `mod` (V.length vec))

newSTM :: Int -> STM (ConcurrentHashTable k v)
newSTM n = do
  count' <- newTVar 0
  vec <- V.replicateM n (newTVar M.empty)
  vecTVar <- newTVar vec
  return $! ConcurrentHashTable count' $! vecTVar

toList :: ConcurrentHashTable k v -> STM [(k, v)]
toList h = do
  vec <- readTVar $ getVector h
  maps <- mapM readTVar $ V.toList vec
  return $ concat $ map M.toList maps

addSTM :: (Hashable k, Ord k) => k -> v -> ConcurrentHashTable k v -> STM ()
addSTM k v h = do
  bucket <- getBucket h k
  m <- readTVar bucket
  let m' = M.insert k v m
  writeTVar bucket $! m'

putSTM :: (Hashable k, Ord k) => k -> v -> ConcurrentHashTable k v -> STM ()
putSTM !k !v h = do
  cnt <- readTVar $ count h
  vec <- readTVar $ getVector h
  let bucketSize = V.length vec
  if cnt * 2 > bucketSize
    then do
      entry <- toList h
      newVec <- V.replicateM (2 * bucketSize) (newTVar M.empty)
      writeTVar (getVector h) newVec
      forM_ entry (\(k', v') -> addSTM k' v' h)
    else return ()
  addSTM k v h
  modifyTVar' (count h) (+ 1)

newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ newSTM 30

getCHT :: (Hashable k, Ord k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT k h = do
 list <- atomically $ do
   bucket <- getBucket h k
   readTVar bucket
 return $! M.lookup k list

putCHT :: (Hashable k, Ord k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT !k !v h = atomically $ putSTM k v h

sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT h = atomically $ readTVar (count h)
