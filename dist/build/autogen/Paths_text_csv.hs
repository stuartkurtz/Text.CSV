module Paths_text_csv (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,1,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/stuart/Library/Haskell/ghc-7.6.3/lib/text-csv-0.1.1.1/bin"
libdir     = "/Users/stuart/Library/Haskell/ghc-7.6.3/lib/text-csv-0.1.1.1/lib"
datadir    = "/Users/stuart/Library/Haskell/ghc-7.6.3/lib/text-csv-0.1.1.1/share"
libexecdir = "/Users/stuart/Library/Haskell/ghc-7.6.3/lib/text-csv-0.1.1.1/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "text_csv_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "text_csv_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "text_csv_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "text_csv_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
