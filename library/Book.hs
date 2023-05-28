module Book where

import qualified ASCII as A
import qualified ASCII.Char as A
import Control.Exception.Safe (tryAny)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Network.Simple.TCP (serve, HostPreference (..))
import qualified Network.Simple.TCP as Net
import Network.Socket (Socket)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S
import Relude
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO

--
-- Chapter 1
--

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

-- Exercise 1 - File resource function

fileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource path mode = do
    allocate (IO.openFile path mode) IO.hClose

writeGreetingSafe' = runResourceT @IO do
    dir <- liftIO getDataDir
    (_releaseKey, h) <- fileResource (dir </> "greeting.txt") WriteMode
    liftIO (IO.hPutStrLn h "hello")
    liftIO (IO.hPutStrLn h "world")

-- Exercise 2 - Showing handles

handlePrintTest = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, fh1) <- fileResource (dir </> "greeting.txt") ReadMode
    (_, fh2) <- fileResource (dir </> "greeting2.txt") WriteMode
    liftIO $ for_ [stdin, stdout, stderr, fh1, fh2] \h -> do
        IO.putStrLn $ show h 
        IO.hShow h >>= IO.putStrLn
        IO.putStrLn ""

-- Exercise 3 - Exhaustion

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

--
-- Chapter 2
--

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

-- Original version
printCapitalizedText h = do proceed
    where
        proceed = do
            chunk <- T.hGetChunk h
            case T.null chunk of
                True -> return ()
                False -> do
                    T.putStr $ T.toUpper chunk
                    proceed

repeatUntilIO ::
    IO chunk                -- ^ Producer of chunks
    -> (chunk -> Bool)      -- ^ Does chunk indicate end of file?
    -> (chunk -> IO ())     -- ^ What to do with each chunk
    -> IO ()
repeatUntilIO getChunk isEnd f = proceed
    where
        proceed = do
            chunk <- getChunk
            case isEnd chunk of
                True -> return ()
                False -> do { f chunk; proceed }

-- Rewrite printFileContentsUpperCase to use repeatUntilIO
printFileContentsUpperCase2 = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
    liftIO $ repeatUntilIO (T.hGetChunk h) T.null (T.putStr . T.toUpper)

-- Exercise 4 - Find the numbers
digitsOnly :: Text -> Text
digitsOnly = T.filter Char.isDigit 

-- Exercise 5 - Capitalize the last
capitalizeLast :: Text -> Text
capitalizeLast txt =
    case T.unsnoc txt of
        Nothing -> txt
        Just (initTxt, c) -> T.snoc initTxt (Char.toUpper c)

-- Exercise 6 - Paren removal
-- Assuming that there are 0 or more layers of balanced parentheses surrounding other characters
-- E.g., "(((cat)))" or "(cat)" or "cat"
unParen :: Text -> Text
unParen txt =
    case T.uncons txt of
        Nothing -> txt
        Just (c, rest) -> 
            if c == '('
                then T.init rest
                else txt

-- Exercise 7 - Character count
characterCount :: FilePath -> IO Int
characterCount fp = do
    dir <- getDataDir
    x <- readChunks (dir </> fp)
    return $ T.length x

readChunks :: FilePath -> IO Text
readChunks fp = runResourceT @IO do 
    (_, handle) <- fileResource fp ReadMode
    liftIO $ go handle (T.pack "")
    where 
        go h chunks = do 
            chunk <- T.hGetChunk h
            case T.null chunk of
                True -> return chunks
                False -> go h (T.append chunk chunks)

-- Exercise 8 - Beyond IO
repeatUntil :: Monad m => m chunk -> (chunk -> Bool) -> (chunk -> m ()) -> m ()
repeatUntil getChunk isEnd f = proceed
    where
        proceed = do
            chunk <- getChunk 
            case isEnd chunk of
                True -> return ()
                False -> do { f chunk; proceed }

printFileContentsUpperCase3 = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
    liftIO $ repeatUntil (T.hGetChunk h) T.null (T.putStr . T.toUpper)

-- Exercise 9 - When and unless
repeatUntil2 :: Monad m => m chunk -> (chunk -> Bool) -> (chunk -> m ()) -> m ()
repeatUntil2 getChunk isEnd f = proceed
    where
        proceed = do
            chunk <- getChunk 
            unless (isEnd chunk) (f chunk >> proceed)

printFileContentsUpperCase4 = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
    liftIO $ repeatUntil2 (T.hGetChunk h) T.null (T.putStr . T.toUpper)

myUnless :: Applicative f => Bool -> f () -> f ()
myUnless b f =
    if not b then f else pure ()

myWhen :: Applicative f => Bool -> f () -> f ()
myWhen b f =
    if b then f else pure ()

--
-- Chapter 3
--

copyGreetingFile = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h1) <- binaryFileResource (dir </> "greeting.txt") ReadMode
    (_, h2) <- binaryFileResource (dir </> "greeting2.txt") WriteMode
    liftIO $ repeatUntil (BS.hGetSome h1 1024) BS.null \chunk -> BS.hPutStr h2 chunk

binaryFileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
binaryFileResource path mode = allocate (IO.openBinaryFile path mode) IO.hClose

exampleBytes = [104, 101, 108, 108, 111] :: [Word8]

helloByteString = do
    IO.hSetBinaryMode stdout True
    BS.hPut stdout (BS.pack helloBytes)

helloBytes = [
    104, 101, 108, 108, 111,        -- hello
    32,                             -- ' '
    119, 111, 114, 108, 100, 33,    -- world!
    10 ]                            -- \n

helloUtf8 = do
    IO.hSetBinaryMode stdout True
    BS.hPutStr stdout (T.encodeUtf8 (T.pack "hello world!\n"))


-- Exercise 10 - A character encoding bug
greet :: ByteString -> IO ()
greet nameBS = case T.decodeUtf8' nameBS of
    Left _ -> putStrLn "Invalid byte string"
    Right nameText -> T.putStrLn (T.pack "Hello, " <> nameText)

-- See https://en.wikipedia.org/wiki/UTF-8 for UTF8 encoding
-- 255 (1111111) is not a valid encoding since there must be a 0 in one of the upper 5 bits
-- (0 in 2 marks continuation, the other postions tell how many bytes the character is)
-- greet $ BS.pack [0xae]

-- Exercise 11 - Byte manipulation
asciiUpper :: ByteString -> ByteString
asciiUpper = BS.map upper
    where
        upper byte = 
            if byte >= 97 && byte <= 122
                then byte - 32
                else byte

-- example: asciiUpper $ T.encodeUtf8 (T.pack "Emmy Noether123")

--
-- Chapter 4
--

makeFriendSafely :: S.SockAddr -> IO ()
makeFriendSafely address = runResourceT @IO do
    (_, s) <- allocate (S.socket S.AF_INET S.Stream S.defaultProtocol) S.close
    liftIO do 
        -- S.setSocketOption s S.UserTimeout 1000   -- not supported on MacOS
        S.connect s address
        S.sendAll s $ T.encodeUtf8 $ T.pack "Hello, will you be my friend?"
        repeatUntil (S.recv s 1024) BS.null BS.putStr
        S.gracefulClose s 1000

findHaskellWebsite :: IO S.AddrInfo
findHaskellWebsite = do
    addrInfos <- S.getAddrInfo
        (Just S.defaultHints { S.addrSocketType = S.Stream })
        (Just "www.haskell.org")
        (Just "http")
    case addrInfos of
        [] -> fail "getAddrInfo returned []"
        x : _ -> return x

makeFriendAddrInfo :: S.AddrInfo -> IO ()
makeFriendAddrInfo addressInfo = runResourceT @IO do
    (_, s) <- allocate (S.openSocket addressInfo) S.close
    liftIO do
        -- S.setSocketOption s S.UserTimeout 1000
        S.connect s (S.addrAddress addressInfo)
        S.sendAll s $ T.encodeUtf8 $ T.pack "Hello, will you be my friend?"
        repeatUntil (S.recv s 1024) BS.null BS.putStr
        S.gracefulClose s 1000

-- Exercise 12 - Improper ResourceT allocation
openAndConnect :: S.AddrInfo -> ResourceT IO (ReleaseKey, Socket)
openAndConnect addressInfo = do
    (releaseKey, s) <- allocate (S.openSocket addressInfo) S.close
    liftIO $ S.connect s (S.addrAddress addressInfo)
    return (releaseKey, s)

makeFriendOpenAndConnect :: S.AddrInfo -> IO ()
makeFriendOpenAndConnect addressInfo = runResourceT @IO do
    (_, s) <- openAndConnect addressInfo
    liftIO do
        S.sendAll s $ T.encodeUtf8 $ T.pack "Hello, will you be my friend?"
        repeatUntil (S.recv s 1024) BS.null BS.putStr
        S.gracefulClose s 1000

-- Exercise 13 - Explore Gopherspace
findGopherSite :: String -> IO S.AddrInfo
findGopherSite host = do
    addrInfos <- S.getAddrInfo
        (Just S.defaultHints { S.addrSocketType = S.Stream })
        (Just host)
        (Just "gopher")
    case addrInfos of
        [] -> fail "getAddrInfo returned []"
        x : _ -> return x

gopherConnect :: S.AddrInfo -> IO ()
gopherConnect addressInfo = runResourceT @IO do
    (_, s) <- openAndConnect addressInfo
    liftIO do
        S.sendAll s $ T.encodeUtf8 $ T.pack ("\r\n")
        repeatUntil (S.recv s 1024) BS.null BS.putStr
        S.gracefulClose s 1000

-- Exercise 14 - Address resolution
resolve :: S.ServiceName -> S.HostName -> IO S.AddrInfo
resolve service host = do
    addrInfos <- S.getAddrInfo
        (Just S.defaultHints { S.addrSocketType = S.Stream })
        (Just host)
        (Just service)
    case addrInfos of
        [] -> fail "getAddrInfo returned []"
        x : _ -> return x

--
-- Chapter 5
--

helloRequestString =
    line [A.string|GET /hello.txt HTTP/1.1|] <>
    line [A.string|User-Agent: curl/7.64.1|] <>
    line [A.string|Accept-Language en, mil|] <>
    line [A.string||]

helloResponseString =
    line [A.string|HTTP/1.1 200 OK|] <>
    line [A.string|Content-Type: text/plain; charset=us-ascii|] <>
    line [A.string|Content-Length: 6|] <>
    line [A.string||] <>
    [A.string|Hello!|]

crlf :: [A.Char]
crlf = [A.CarriageReturn, A.LineFeed]

line :: ByteString -> ByteString
line x = x <> A.fromCharList crlf 

ourFirstServer = serve @IO HostAny "8000" \(s, a) -> do
    putStrLn ("New connection from " <> show a)
    Net.send s helloResponseString

-- Exercise 15 - Repeat until Nothing
repeatUntilNothing :: Monad m => m (Maybe chunk) -> (chunk -> m ()) -> m ()
repeatUntilNothing getChunkMaybe f = proceed
    where
        proceed = do
            chunk <- getChunkMaybe 
            case  chunk of
                Nothing -> return ()
                Just c -> do { f c; proceed }

-- Exercise 16 - Make an HTTP reuqest
-- Defined a general makeRequest function to send a request to a server/protocol specified
-- in the AddressInfo argument
-- Use testHttpRequest to test this function with HTTP request to retrieve root
-- Can Use it for the gopher: 
--    resolve "gopher" "quux.org" >>= \ai -> makeRequest ai (T.encodeUtf8 $ T.pack ("\r\n"))
makeRequest :: S.AddrInfo -> ByteString -> IO ()
makeRequest addressInfo request = runResourceT @IO do
    (_, s) <- openAndConnect addressInfo
    liftIO do
        Net.send s request
        repeatUntilNothing (Net.recv s 1024) BS.putStr
        S.gracefulClose s 1000

-- Exercise 17 - Test the hello server
-- In addition to testing the server with a browser (http://127.0.0.1:8000) and
-- curl -v http://127.0.0.1:8000, can use testHttpRequest "127.0.0.1" "8000"
testHttpRequest :: S.HostName -> S.ServiceName -> IO ()
testHttpRequest host port = do
    addrInfo <- resolve port host
    makeRequest addrInfo request
    where
        request =
            line [A.string|GET / HTTP/1.1|] <>
            line [A.string|User-Agent: curl/7.64.1|] <>
            line [A.string|Accept-Language en, mil|] <>
            line [A.string||]



