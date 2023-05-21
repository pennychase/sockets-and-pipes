module Book where

import Control.Exception.Safe (tryAny)
import Relude
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO

-- Creates (if doesn't exist) and returns the FilePath of the directory where data files will be stored
-- Uses the XDG Base Directory Specification
getDataDir :: IO FilePath
getDataDir = do
    dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
    Dir.createDirectoryIfMissing True dir
    return dir

-- Write to a file
writeGreetingFile :: IO ()
writeGreetingFile = do
    dir <- getDataDir
    h <- IO.openFile (dir </> "greeting.txt") WriteMode
    IO.hPutStrLn h "hello"
    IO.hPutStrLn h "world"
    IO.hClose h

-- Handle exceptions with tryAny
writeGreetingTry :: IO ()
writeGreetingTry = do
    dir <- getDataDir
    IO.hPutStrLn IO.stderr "About to open the file :/"
    openResult <- tryAny $ IO.openFile (dir </> "greeting.txt") WriteMode
    case openResult of
        Left _ -> IO.hPutStrLn IO.stderr "Cannot open file to write ):"
        Right h -> do
            IO.hPutStrLn h "hello"
            IO.hPutStrLn h "world"
            IO.hClose h
            IO.hPutStrLn IO.stderr "Done :)"
