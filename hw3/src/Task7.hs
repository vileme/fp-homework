{-# LANGUAGE RecordWildCards #-}

module Task7
  ( changeExtension
  , getAllNames
  , rmIfEmpty
  ) where

import           Lens.Micro      (filtered, traversed, (%~), (^.), (^..), _2)
import           System.FilePath (replaceExtension)
import           Task5           (FS (..), dir, name)

isFile :: FS -> Bool
isFile File {..} = True
isFile _         = False

changeExtension :: FilePath -> FS -> FS
changeExtension extension = dir . _2 . traversed . filtered isFile . name %~ flip replaceExtension extension

getAllNames :: FS -> [FilePath]
getAllNames fp = allCurrentNames ++ allRecursiveNames
  where
    allCurrentNames :: [FilePath]
    allCurrentNames = fp ^.. dir . _2 . traversed . name
    allRecursiveNames :: [FilePath]
    allRecursiveNames = concatMap getAllNames $ fp ^.. dir . _2 . traversed . filtered (not . isFile)

rmIfEmpty :: FilePath -> FS -> FS
rmIfEmpty directory = dir . _2 %~ filter notEmpty
  where
    nameMatch :: FS -> Bool
    nameMatch fs = fs ^. name == directory
    notEmpty :: FS -> Bool
    notEmpty fs = not (not (isFile fs) && nameMatch fs)
