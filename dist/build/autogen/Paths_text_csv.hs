module Paths_text_csv (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,2,6], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Volumes/Home/stuart/Library/Haskell/bin"
libdir     = "/Volumes/Home/stuart/Library/Haskell/ghc-7.8.3-x86_64/lib/text-csv-0.1.2.6"
datadir    = "/Volumes/Home/stuart/Library/Haskell/share/ghc-7.8.3-x86_64/text-csv-0.1.2.6"
libexecdir = "/Volumes/Home/stuart/Library/Haskell/libexec"
sysconfdir = "/Volumes/Home/stuart/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "text_csv_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "text_csv_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "text_csv_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "text_csv_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "text_csv_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
