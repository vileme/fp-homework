{-# LANGUAGE RecordWildCards #-}
module Task7
 (
   changeExtension
 , getAllNames
 , rmIfEmpty)
 where

import Lens.Micro (_2, traversed, filtered, (%~), (&))
import System.FilePath (replaceExtension)
import Task5 (FS(..), dir, name)

isFile :: FS -> Bool
isFile File {..} =  True
isFile Dir {..} = False

changeExtension :: String -> FS -> FS
changeExtension extension  = dir . _2 . traversed . filtered isFile . name %~ flip replaceExtension extension

getAllNames = undefined
rmIfEmpty = undefined
