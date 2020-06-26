{-# LANGUAGE Rank2Types #-}
module Task5
  (
  dir
  , fileName
  , getFS
  , name
  , FS (..) 
  )
 where

import System.Directory
import System.FilePath (combine, splitDirectories, takeFileName)
import Lens.Micro

data FS
  = Dir
      { _name     :: FilePath
      , _contents :: [FS]
      }
  | File
      { _name :: FilePath
      }
  deriving Show

getFS :: FilePath -> IO FS
getFS p = do
  path <- doesPathExist p
  if path
    then do
      directory <- doesDirectoryExist p
      if directory
        then Dir (last $ splitDirectories p) <$> getContentsFromDir p
        else return $ File p
    else ioError $ userError "given path doesn't exist"
  where
    getContentsFromDir :: FilePath -> IO [FS]
    getContentsFromDir directory = do
      contentsNames <- listDirectory directory
      let contentRelativeNames = map (combine directory) contentsNames
      mapM dfs contentRelativeNames
      where dfs :: FilePath -> IO FS
            dfs pt = do
              d <- doesDirectoryExist pt
              if d
                 then Dir (last $ splitDirectories pt) <$> getContentsFromDir pt
                 else return $ File $ takeFileName pt


name :: Lens' FS FilePath
name = lens _name (\fs v -> fs {_name = v})

dir :: Traversal' FS (FilePath, [FS])
dir f (Dir dname dcontents) = uncurry Dir <$> f (dname, dcontents)
dir _ f = pure f

fileName :: Traversal' FS FilePath
fileName f (File x) = File <$> f x
fileName _ fs = pure fs

