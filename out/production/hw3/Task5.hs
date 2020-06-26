module Task5 where
import System.FilePath
import System.Directory
data FS
    = Dir
    { name :: FilePath
    , contents :: [FS]
    }
    | File
    { name :: FilePath
    }
getFS :: FilePath -> IO FS
getFS path = if doesPathExist path
                then return ()
                else ioError $ userError "given path doesn't exist"
