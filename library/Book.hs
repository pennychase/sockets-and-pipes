module Book where

import Control.Exception.Safe (tryAny)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import qualified Data.Char as Char
import Relude
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified Data.Text as T
import qualified Data.Text.IO as T

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

-- Ensure file handles are closed using 
writeGreetingSafe = runResourceT @IO do
    dir <- liftIO getDataDir
    (_releaseKey, h) <- allocate (IO.openFile (dir </> "greeting.txt") WriteMode) IO.hClose
    liftIO (IO.hPutStrLn h "hello")
    liftIO (IO.hPutStrLn h "world")

--
-- Chapter 1 Exercises
--

-- Exercise 1.1 - File resource function

fileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource path mode = do
    allocate (IO.openFile path mode) IO.hClose

writeGreetingSafe' = runResourceT @IO do
    dir <- liftIO getDataDir
    (_releaseKey, h) <- fileResource (dir </> "greeting.txt") WriteMode
    liftIO (IO.hPutStrLn h "hello")
    liftIO (IO.hPutStrLn h "world")

-- Exercise 1.2 - Showing handles

handlePrintTest :: IO ()
handlePrintTest = do
    dir <- getDataDir
    h <- IO.openFile (dir </> "greeting.txt") ReadMode
    putStrLn $ show h
    IO.hShow h >>= putStrLn

-- Exercise 1.3 - Exhaustion

howManyHandles = runResourceT @IO do
    hs <- openManyHandles 
    putStrLn ("Opened " <> show (length hs) <> " handles")

openManyHandles :: ResourceT IO [Handle]
openManyHandles = do
    go []
    where 
        go hs = do
            result <- fileResourceMaybe
            case result of
                Just h -> go (h:hs)
                Nothing -> return hs

fileResourceMaybe :: ResourceT IO (Maybe Handle)
fileResourceMaybe = do
    dir <- liftIO getDataDir
    result <- tryAny $ allocate (IO.openFile (dir </> "greeting.txt") ReadMode) IO.hClose
    case result of
        Right x -> return $ Just (snd x)
        Left e -> do
            print (displayException e)
            return Nothing


-- Writing Text to stdout
helloText = T.hPutStrLn stdout (T.pack "hello world")

-- Writing Text to a file
helloTextFile = runResourceT @IO do
    dir <- liftIO getDataDir 
    (_, h) <- fileResource (dir </> "greeting.txt") WriteMode
    liftIO do
        T.hPutStrLn h (T.pack "hello")
        T.hPutStrLn h (T.pack "world")

-- Reading from a file in chunks, convert to upper case, and write to stdout
printFileContentsUpperCase = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
    liftIO (printCapitalizedText h)

printCapitalizedText h = proceed
    where
        proceed = do
            chunk <- T.hGetChunk h
            case T.null chunk of
                True -> return ()
                False -> do
                    T.putStr $ T.toUpper chunk
                    proceed




