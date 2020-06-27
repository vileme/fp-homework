{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}

module Task6
  ( ls
  , cd
  , file
  ) where

import           Lens.Micro (Traversal', failing, filtered, traversed, (^.), _2)
import           Task5      (FS (..), dir, fileName, name)

ls :: Traversal' FS FilePath
ls = dir . _2 . traversed . name

cd :: FilePath -> Traversal' FS FS
cd path = dir . _2 . traversed . filtered (matchName path)
  where
    matchName :: FilePath -> FS -> Bool
    matchName _ File {..} = False
    matchName p d         = d ^. name == p

file :: FilePath -> Traversal' FS FilePath
file fname = failing fileTraverse dirTraverse
  where
    fileTraverse :: Traversal' FS FilePath
    fileTraverse = fileName . filtered (== fname)
    dirTraverse :: Traversal' FS FilePath
    dirTraverse = dir . _2 . traversed . fileName . filtered (== fname)
