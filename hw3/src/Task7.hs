{-# LANGUAGE RecordWildCards #-}
module Task7
 (
   changeExtension
 , getAllNames
 , rmIfEmpty)
 where

import Lens.Micro (_2, traversed, filtered, (%~),(^..), (^.))
import System.FilePath (replaceExtension)
import Task5 (FS(..), dir, name)

isFile :: FS -> Bool
isFile File {..} =  True
isFile _  = False

changeExtension :: FilePath -> FS -> FS
changeExtension extension  = dir . _2 . traversed . filtered isFile . name %~ flip replaceExtension extension

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
    isDir :: FS -> Bool
    isDir fs = not $ isFile fs
    nameMatch :: FS -> Bool
    nameMatch fs = fs ^. name == directory
    notEmpty :: FS -> Bool
    notEmpty fs = not (isDir fs && nameMatch fs)
      
