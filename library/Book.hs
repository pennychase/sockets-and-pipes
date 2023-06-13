module Book where

import qualified ASCII as A
import qualified ASCII.Char as A
import ASCII.Decimal (Digit (..))
import Control.Exception.Safe (tryAny)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Time as Time
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
    line [A.string|Accept-Language: en, mil|] <>
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

-- Exercise 16 - Make an HTTP request
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

testHttpRequest :: S.HostName -> S.ServiceName -> ByteString -> IO ()
testHttpRequest host port request = do
    addrInfo <- resolve port host
    makeRequest addrInfo request

haskellRequestString =
    line [A.string|GET / HTTP/1.1|] <>
    line [A.string|Host: haskell.org|] <>
    line [A.string|Connection: close|] <>
    line [A.string||]

-- Exercise 17 - Test the hello server
-- In addition to testing the server with a browser (http://127.0.0.1:8000) and
-- curl -v http://127.0.0.1:8000, can use:
--     testHttpRequest "127.0.0.1" "8000" helloRequestString


--
-- Chapter 6
--

-- Request
data Request = Request RequestLine [Field] (Maybe Body)
            deriving Show

data RequestLine = RequestLine Method RequestTarget Version
            deriving Show

-- RFC 9112 enumerates 8 request methods, but says the list is not exhaustive, so instead
-- of defining Method as an enumeration we define it as an ASCII ByteString
data Method = Method (A.ASCII ByteString)
            deriving Show

-- Define RequestTarget as an opaque ASCII ByteString, ignoring the complexity for now
data RequestTarget = RequestTarget (A.ASCII ByteString)
                deriving Show

-- Response
data Response = Response StatusLine [Field] (Maybe Body)
            deriving Show

data StatusLine = StatusLine Version StatusCode (Maybe ReasonPhrase)
            deriving Show

data StatusCode = StatusCode Digit Digit Digit
            deriving Show

data ReasonPhrase = ReasonPhrase (A.ASCII ByteString)
            deriving Show

-- Common types for Requests and Responses
data Version = Version Digit Digit
            deriving Show

data Field = Field FieldName FieldValue
            deriving Show

data FieldName = FieldName (A.ASCII ByteString)
            deriving Show

data FieldValue = FieldValue (A.ASCII ByteString)
            deriving Show

data Body = Body LByteString
            deriving Show

-- Exercise 18 - Construct some values

helloRequest :: Request
helloRequest = Request start [host, lang] Nothing
    where
        start = RequestLine (Method [A.string|GET|])
                            (RequestTarget [A.string|/hello.txt|])
                            (Version Digit1 Digit1)
        host = Field (FieldName [A.string|Host|])
                     (FieldValue [A.string|wwww.example.com|])
        lang = Field (FieldName [A.string|Accept-Language|])
                     (FieldValue [A.string|en, mi|])

helloResponse :: Response
helloResponse = Response status [contType, contLength] (Just body)
    where
        status = StatusLine (Version Digit1 Digit1)
                            (StatusCode Digit2 Digit0 Digit0)
                            (Just (ReasonPhrase [A.string|OK|]))
        contType = Field (FieldName [A.string|Content-Type|])
                         (FieldValue [A.string|text/plain; charset=us-ascii|])
        contLength = Field (FieldName [A.string|Content-Length|])
                           (FieldValue [A.string|6|])
        body = Body [A.string|Hello!|]

-- Exercise 19 - Infinite byte strings

lazyStringPrefix :: String -> Int64 -> LByteString
lazyStringPrefix start n = LBS.take n $ LBS.cycle (LBS.fromStrict (T.encodeUtf8 (T.pack start)))


--
-- Chapter 7
--

-- printHelloBuilder (T.pack "Alonzo")
printHelloBuilder name = LT.putStrLn $ TB.toLazyText $
    TB.fromString "Hello " <> TB.fromText name <> TB.fromString "!"

time :: IO () -> IO ()
time action = do
    a <- Time.getCurrentTime
    action
    b <- Time.getCurrentTime
    print (Time.diffUTCTime b a)

concatWithStrict :: Int -> Text
concatWithStrict numberOfTimes = fold $ replicate numberOfTimes $ T.pack "a"

concatWithBuilder :: Int -> Text
concatWithBuilder numberOfTimes = LT.toStrict $ TB.toLazyText $
    fold $ replicate numberOfTimes $ TB.fromString "a"

-- write strict text and builder text to files to ensure the entire texts that are created
-- are evaluated (I/O that uses the result ensures evaluation)
concatSpeedTest :: Int -> IO ()
concatSpeedTest n = do
    dir <- getDataDir
    time $ T.writeFile (dir </> "strict.text") (concatWithStrict n)

-- Encdoing requests and response
-- To print nicely in GHCI:
--    BSB.hPutBuilder IO.stdout $ encodeRequest helloRequest
--    BSB.hPutBuilder IO.stdout $ encodeResponse helloResponse

{-
HTTP message format from RFC 9112:

HTTP-message =  start-line
                *( field-line CRLF) 
                CRLF
                [ message-body ]
-}

-- Request
encodeRequest :: Request -> BSB.Builder
encodeRequest (Request requestLine fields bodyMaybe) =
    encodeRequestLine requestLine
    <> repeatedlyEncode (\x -> encodeField x <> encodeLineEnd) fields
    <> encodeLineEnd
    <> optionallyEncode encodeBody bodyMaybe

{-
Request start-line from RFC 9112:
    request-line =  method SP request-target SP HTTP-version CRLF
-}
encodeRequestLine :: RequestLine -> BSB.Builder
encodeRequestLine (RequestLine method target version) =
    encodeMethod method <> A.fromCharList [A.Space]
    <> encodeRequestTarget target <> A.fromCharList [A.Space]
    <> encodeVersion version
    <> encodeLineEnd

encodeMethod :: Method -> BSB.Builder
encodeMethod (Method x) = BSB.byteString (A.lift x)

encodeRequestTarget :: RequestTarget -> BSB.Builder
encodeRequestTarget (RequestTarget x) = BSB.byteString (A.lift x)

-- Response
encodeResponse :: Response -> BSB.Builder
encodeResponse (Response statusLine fields bodyMaybe) =
    encodeStatusLine statusLine
    <> repeatedlyEncode (\x -> encodeField x <> encodeLineEnd) fields
    <> encodeLineEnd
    <> optionallyEncode encodeBody bodyMaybe

{-
Response status-line from RFC 9112:
    status-line = HTTP-version SP status-code SP [ reason-phrase ] CRLF
    status-code = 3DIGIT
    reason-phrase = 1+ ( HTAB / SP / VCHAR)
-}
encodeStatusLine :: StatusLine -> BSB.Builder
encodeStatusLine (StatusLine version code reason) = 
    encodeVersion version <> A.fromCharList [A.Space]
    <> encodeStatusCode code <> A.fromCharList [A.Space]
    <> optionallyEncode encodeReasonPhrase reason
    <> encodeLineEnd

encodeStatusCode :: StatusCode -> BSB.Builder
encodeStatusCode (StatusCode x y z) = A.fromDigitList [x, y, z]

encodeReasonPhrase :: ReasonPhrase -> BSB.Builder
encodeReasonPhrase (ReasonPhrase x) = BSB.byteString (A.lift x)

-- Common

encodeLineEnd :: BSB.Builder
encodeLineEnd = A.fromCharList crlf

encodeVersion :: Version -> BSB.Builder
encodeVersion (Version x y) = 
    [A.string|HTTP/|] <> A.fromDigitList [x] <> [A.string|.|] <> A.fromDigitList [y]

repeatedlyEncode :: (a -> BSB.Builder) -> [a] -> BSB.Builder
repeatedlyEncode = foldMap

optionallyEncode :: (a -> BSB.Builder) -> Maybe a -> BSB.Builder
optionallyEncode = foldMap

-- Exercise 20 - Field encoding

{-
Field-line from RFC 9112 and components from RFC 9110:
    field-line =    field-name ":" OWS field-value OWS
    field-name =    1*tchar
    field-value =   *field-content
    field-content = field-vchar [1*( SP / HTAB / field-vchar) field-vchar ]
    field-vchar =   VCHAR / obs-text
    tchar =         any VCHAR except delimiters
-}
encodeField :: Field -> BSB.Builder
encodeField (Field fname fvalue) =
    encodeFieldName fname
    <> [A.string|:|] <> A.fromCharList [A.Space]
    <> encodeFieldValue fvalue

encodeFieldName :: FieldName -> BSB.Builder
encodeFieldName (FieldName x) = BSB.byteString (A.lift x)

encodeFieldValue :: FieldValue -> BSB.Builder
encodeFieldValue (FieldValue x) = BSB.byteString (A.lift x)

-- Exercise 21 - Message body encoding
encodeBody :: Body -> BSB.Builder
encodeBody (Body x) = BSB.lazyByteString x

--
-- Chapter 8
--

-- To print nicely in GHCI (since this is small, ok to make LByteString strict):
--    T.putStrLn $ (T.decodeUtf8 . LBS.toStrict . A.lift . countHelloAscii) 1
-- if large body, import Data.Text.Lazy.Encoding:
--    LT.putStrLn $ (LT.decodeUtf8 . A.lift  . countHelloAscii) 1
countHelloAscii :: Natural -> A.ASCII LByteString
countHelloAscii count = 
    [A.string|Hello!|] <> A.fromCharList crlf <> case count of
        0 -> [A.string|This page has never been viewed.|]
        1 -> [A.string|This page has been viewed 1 time.|]
        _ -> [A.string|This page has been viewed |]
                <> A.showIntegralDecimal count
                <> [A.string| times.|]


-- Response building utilities

-- Status Line

data Status = Status StatusCode (Maybe ReasonPhrase)

ok :: Status
ok = Status (StatusCode Digit2 Digit0 Digit0) (Just (ReasonPhrase [A.string|OK|]))

http_1_1 :: Version
http_1_1 = Version Digit1 Digit1

status :: Status -> StatusLine
status (Status code phrase) = StatusLine http_1_1 code phrase

-- Fields

contentType = FieldName [A.string|Content-Type|]

plainAscii = FieldValue [A.string|text/plain; charset=us-ascii|]

contentLength = FieldName [A.string|Content-Length|]

-- Turn ASCII message body into OK response
asciiOk :: A.ASCII LByteString -> Response
asciiOk str = Response (status ok) [typ, len] (Just body)
    where
        typ = Field contentType plainAscii
        len = Field contentLength (bodyLengthValue body)
        body = Body (A.lift str)

bodyLengthValue :: Body -> FieldValue
bodyLengthValue (Body x) = FieldValue (A.showIntegralDecimal (LBS.length x))

-- Sending Response

sendResponse :: Socket -> Response -> IO ()
sendResponse s r = Net.sendLazy s $ BSB.toLazyByteString (encodeResponse r)

-- Server (count not incremented)

stuckCountingServer = serve @IO HostAny "8000" \(s, _) -> do
    let count = 0 
    sendResponse s (asciiOk (countHelloAscii count))

-- Exercise 23

{-
In ghci start the server: stuckCountingServer

In a terminal, run: curl --http1.1 --dump-header - http://localhost:8000
-}

-- Exercise 24

mid :: Word8 -> Word8 -> Word8
mid x y = div (x + y) 2

{-
For the input 210 amd 230, the sum is greater than maxBound :: Word8. So convert to Integer for the operations.
-}

mid' :: Word8 -> Word8 -> Word8
mid' x y = fromInteger (div ((toInteger x) + (toInteger y)) 2)