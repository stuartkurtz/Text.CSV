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
version = Version [0,1,2,6] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/stuart/Library/Haskell/bin"
libdir     = "/Users/stuart/Library/Haskell/ghc-7.10.2-x86_64/lib/text-csv-0.1.2.6"
datadir    = "/Users/stuart/Library/Haskell/share/ghc-7.10.2-x86_64/text-csv-0.1.2.6"
libexecdir = "/Users/stuart/Library/Haskell/libexec"
sysconfdir = "/Users/stuart/Library/Haskell/etc"

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
