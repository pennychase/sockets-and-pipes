module Book where

import qualified ASCII as A
import qualified ASCII.Char as A
import ASCII.Decimal (Digit (..))
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe (tryAny)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as J.Key
import qualified Data.Aeson.KeyMap as J.KeyMap
import Data.Aeson (ToJSON (toJSON), (.=))
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Run as P
import Data.Attoparsec.ByteString (Parser, (<?>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Time as Time
import Network.Simple.TCP (serve, HostPreference (..))
import qualified Network.Simple.TCP as Net
import Network.Socket (Socket)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as S
import Pipes (Producer, Consumer, Pipe, (>->), yield, await, runEffect)
import Relude
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.IO as IO
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as HTML
import Unfork (unforkAsyncIO_)

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
    line [A.string|Accept-Language: en, mi|] <>
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
            deriving (Eq, Show)

data RequestLine = RequestLine Method RequestTarget Version
            deriving (Eq, Show)

-- RFC 9112 enumerates 8 request methods, but says the list is not exhaustive, so instead
-- of defining Method as an enumeration we define it as an ASCII ByteString
data Method = Method (A.ASCII ByteString)
            deriving (Eq, Show)

-- Define RequestTarget as an opaque ASCII ByteString, ignoring the complexity for now
data RequestTarget = RequestTarget (A.ASCII ByteString)
                deriving (Eq, Show)

-- Response
data Response = Response StatusLine [Field] (Maybe Body)
            deriving (Eq, Show)

data StatusLine = StatusLine Version StatusCode (Maybe ReasonPhrase)
            deriving (Eq, Show)

data StatusCode = StatusCode Digit Digit Digit
            deriving (Eq, Show)

data ReasonPhrase = ReasonPhrase (A.ASCII ByteString)
            deriving (Eq, Show)

-- Common types for Requests and Responses
data Version = Version Digit Digit
            deriving (Eq, Show)

data Field = Field FieldName FieldValue
            deriving (Eq, Show)

data FieldName = FieldName (A.ASCII ByteString)
            deriving (Eq, Show)

data FieldValue = FieldValue (A.ASCII ByteString)
            deriving (Eq, Show)

data Body = Body LByteString
            deriving (Eq, Show)

-- Exercise 18 - Construct some values

helloRequest :: Request
helloRequest = Request start [host, lang] Nothing
    where
        start = RequestLine (Method [A.string|GET|])
                            (RequestTarget [A.string|/hello.txt|])
                            (Version Digit1 Digit1)
        host = Field (FieldName [A.string|Host|])
                     (FieldValue [A.string|www.example.com|])
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

-- Exercise 22 - Encoding test

{-
To see the encodings:

BSB.toLazyByteString (encodeResponse helloResponse)
BSB.toLazyByteString (encodeRequest helloRequest)

To test equality against hand-crafted strings from Chapter 5, we have to convert since the hand-crafted 
strings are strict and the BSB.Builder produced by the encoders are converted to LazyByteString by
BSB.toLazyByteString

BSB.toLazyByteString (encodeResponse helloResponse) == LBS.fromStrict helloResponseString
>> True

BSB.toLazyByteString (encodeRequest helloRequest) == LBS.fromStrict helloRequestString
>> False
The encoder has Host field and the hand-crafted string has User-Agent. If we create a new handcrafted string
they will be identical
-}
helloRequestString' =
    line [A.string|GET /hello.txt HTTP/1.1|] <>
    line [A.string|Host: www.example.com|] <>
    line [A.string|Accept-Language: en, mi|] <>
    line [A.string||]

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

-- Exercise 23 - Read the header

{-
In ghci start the server: stuckCountingServer

In a terminal, run: curl --http1.1 --dump-header - http://localhost:8000
-}

-- Exercise 24 - Overflow

mid :: Word8 -> Word8 -> Word8
mid x y = div (x + y) 2

{-
For the input 210 amd 230, the sum is greater than maxBound :: Word8.
Fix it by converting to Integer for the operations and converting back to Word*
-}

mid' :: Word8 -> Word8 -> Word8
mid' x y = fromInteger (div ((toInteger x) + (toInteger y)) 2)

--
-- Chapter 9
--

-- FieldValue constants for Content-Type

plainUtf8 :: FieldValue
plainUtf8 = FieldValue [A.string|text/plain; charset=utf-8|]

htmlUtf8 :: FieldValue
htmlUtf8 = FieldValue [A.string|text/html; charset=utf8|]

json :: FieldValue
json = FieldValue [A.string|application/json|]

countHelloText :: Natural -> LText
countHelloText count = TB.toLazyText $
    TB.fromString "Hello! \9835\r\n" <>
    case count of
        0 -> TB.fromString "This page has never been viewed\r\n"
        1 -> TB.fromString "This page has been viewed 1 time\r\n"
        _ -> TB.fromString "This page has been viewed " <> 
                            TB.decimal count <> TB.fromString " times\r\n"

textOk :: LText -> Response
textOk str = Response (status ok) [typ, len] (Just body)
    where
        typ = Field contentType plainUtf8
        len = Field contentLength (bodyLengthValue body)
        body = Body (LT.encodeUtf8 str)

stuckCountingServerText = serve @IO HostAny "8000" \(s, _) -> do
    let count = 0
    sendResponse s (textOk (countHelloText count))

-- HTML

countHelloHtml :: Natural -> Html
countHelloHtml count = HTML.docType <> htmlDocument
    where
        htmlDocument = HTML.html $ documentMetadata <> documentBody

        documentMetadata = HTML.head titleHtml
        titleHtml = HTML.title (toHtml "My great web page")

        documentBody = HTML.body $ greetingHtml <> HTML.hr <> hitCounterHtml
        greetingHtml = HTML.p (toHtml "Hello! \9835")
        hitCounterHtml = HTML.p $ case count of
            0 -> toHtml "This page has never been viewed"
            1 -> toHtml "This page has been viewed 1 time"
            _ -> toHtml "This page has been viewed " <> toHtml @Natural count <> toHtml " times"

-- JSON

-- verbose form
countHelloJSON1 :: Natural -> J.Value
countHelloJSON1 count = toJSON (J.KeyMap.fromList [greetingJSON, hitsJSON])
    where
        greetingJSON = (J.Key.fromString "greeting", toJSON "Hello! \9835")

        hitsJSON = ( J.Key.fromString "hits"
                   , toJSON (J.KeyMap.fromList [numberJSON, messageJSON]))

        numberJSON = (J.Key.fromString "count", toJSON count)

        messageJSON = (J.Key.fromString "message", toJSON (countHelloText count))

-- Aeson shortcuts
countHelloJSON :: Natural -> J.Value
countHelloJSON count = J.object [
    fromString "greeting" .= fromString @Text "Hello! \9835",
    fromString "hits" .= J.object [
        fromString "counts" .= count,
        fromString "message" .= countHelloText count ] ]

-- JSON Response
jsonOk :: J.Value -> Response
jsonOk str = Response (status ok) [typ, len] (Just body)
    where
        typ = Field contentType json
        len = Field contentLength (bodyLengthValue body)
        body = Body (J.encode str)

        
-- Exercise 25 - HTML in the browser

htmlOk :: Html -> Response
htmlOk str = Response (status ok) [typ, len] (Just body)
    where
        typ = Field contentType htmlUtf8
        len = Field contentLength (bodyLengthValue body)
        body = Body (renderHtml str)

stuckCountingServerHtml = serve @IO HostAny "8000" \(s, _) -> do
    let count = 0
    sendResponse s (htmlOk (countHelloHtml count))

-- Exercise 26 - Type ambiguity
{-
Remove type annotation in countHelloJSON. Error message says there's an ambiguous type
and suggests adding a type annotation.
-}

--Exercise 27 - Encoding with class

class Encode a where
    encode :: a -> BSB.Builder

instance Encode Request where
    encode = encodeRequest

instance Encode Response where
    encode = encodeResponse

instance Encode Integer where
    encode = BSB.integerDec

instance Encode Int64 where
    encode = BSB.int64Dec

instance Encode Text where
    encode = BSB.byteString . T.encodeUtf8

instance Encode LText where
    encode = BSB.lazyByteString . LT.encodeUtf8

instance Encode ByteString where
    encode = BSB.byteString 

instance Encode LByteString where
    encode = BSB.lazyByteString

instance Encode BSB.Builder where
    encode a = a


--
-- Chapter 10
--

countingServer :: IO ()
countingServer = do
    hitCounter <- atomically (newTVar @Natural 0)
    serve @IO HostAny "8000" \(s, _) -> do
        count <- atomically (increment hitCounter)
        sendResponse s (textOk (countHelloText count))

increment :: TVar Natural -> STM Natural
increment hitCounter = do
    oldCount <- readTVar hitCounter
    let newCount = oldCount + 1
    writeTVar hitCounter newCount
    return newCount

{-
STM Queue

import qualified Control.Concurrent.STM as STM

q <- STM.atomically (STM.newTQueue @(STM (STM.TQueue Int)))
STM.atomically (STM.writeTQueue q 1) --  [1]
STM.atomically (STM.writeTQueue q 2) -- [1,2]
STM.atomically (STM.readTQueue q) => 1
STM.atomically (STM.readTQueue q) => 2
STM.atomically (STM.isEmptyTQueue) => True

-}

-- Exercise 28 - Interleaving

incrementNotAtomic :: TVar Natural -> IO Natural
incrementNotAtomic hitCounter = do
    oldCount <- atomically (readTVar hitCounter)
    let newCount = oldCount + 1
    atomically (writeTVar hitCounter newCount)
    return newCount 

testIncrement :: (TVar Natural -> IO a) -> IO Natural
testIncrement inc = do
    x <- atomically (newTVar @Natural 0)
    Async.replicateConcurrently_ 10 (replicateM 1000 (inc x))
    atomically (readTVar x)

-- Exercise 29 - Times gone by

timingServer :: IO ()
timingServer = do
    lastTime <- atomically (newTVar @(Maybe Time.UTCTime) Nothing)
    serve @IO HostAny "8000" \(s, _) -> do
        now <- Time.getCurrentTime
        diffTime <- atomically (getDiffTime lastTime now)
        sendResponse s $ textOk $ LT.pack $
            show (diffTime :: Maybe Time.NominalDiffTime)

getDiffTime :: TVar (Maybe Time.UTCTime) -> Time.UTCTime -> STM (Maybe Time.NominalDiffTime)
getDiffTime lastTime now = do
    maybeOldTime <- readTVar lastTime
    writeTVar lastTime (Just now)
    return $ Time.diffUTCTime now <$> maybeOldTime      -- Use fmap
    -- return $ case maybeOldTime of                    -- Use case
    --     Nothing -> Nothing
    --     Just oldTime -> Just $ Time.diffUTCTime now oldTime


--
-- Chapter 11
--

hContentsResponse :: Handle -> IO Response
hContentsResponse h = do
    fileContent <-  liftIO (LBS.hGetContents h)
    let body = Just (Body fileContent)
    return (Response (status ok) [] body)
   
fileStrict = do
    dir <- getDataDir
    serve @IO HostAny "8000" \(s, _) -> runResourceT @IO do
        (_, h) <- binaryFileResource (dir </> "stream.txt") ReadMode
        r <- liftIO (hContentsResponse h)
        liftIO (sendResponse s r)

-- Chunks

data Chunk = Chunk ChunkSize ChunkData

data ChunkData = ChunkData ByteString

data ChunkSize = ChunkSize Natural

dataChunk :: ChunkData -> Chunk
dataChunk chunkData = Chunk (chunkDataSize chunkData) chunkData

chunkDataSize :: ChunkData -> ChunkSize
chunkDataSize (ChunkData bs) = case toIntegralSized @Int @Natural (BS.length bs) of
    Just n -> ChunkSize n
    Nothing -> error (T.pack "BS.length is always Natural")

-- Chunk Encoding

{-
Chunk Transfer Encoding from RFC 9112 (omits chunk extensions)

chunked-body    = *chunk last-chunk trailer-section CRLF
chunk           =  chunk-size CRLF chunk-data CRLF
chunk-size      = 1*HEXDIG
last-chunk      = 1*("0") CRLF
chunk-data      = 1*OCTET ; a sequence of chunk-size octets
-}

encodeChunk :: Chunk -> BSB.Builder
encodeChunk (Chunk chunkSize chunkData) =
    encodeChunkSize chunkSize <> encodeLineEnd <>
    encodeChunkData chunkData <> encodeLineEnd

encodeChunkSize :: ChunkSize -> BSB.Builder
encodeChunkSize (ChunkSize x) = A.showIntegralHexadecimal A.LowerCase x

encodeLastChunk :: BSB.Builder
encodeLastChunk = encodeChunkSize (ChunkSize 0) <> encodeLineEnd

encodeChunkData :: ChunkData -> BSB.Builder
encodeChunkData (ChunkData bs) = BSB.byteString bs

-- constants for Transfer-Encoding

transferEncoding = FieldName [A.string|"Transfer-Encoding"|]
chunked = FieldValue [A.string|chunked|]
transferEncodingChunked = Field transferEncoding chunked

-- serving the file

fileStreaming :: IO ()
fileStreaming = do
    dir <- getDataDir
    serve @IO HostAny "8000" \(s, _) -> runResourceT @IO do
        (_, h) <- binaryFileResource (dir </> "stream.txt") ReadMode
        liftIO do
            sendBSB s (encodeStatusLine (status ok))
            sendBSB s (encodeFieldList [transferEncodingChunked])
            repeatUntil (BS.hGetSome h 1024) BS.null \c ->
                sendBSB s (encodeChunk (dataChunk (ChunkData c)))
            sendBSB s encodeLastChunk
            sendBSB s (encodeFieldList [])
            
sendBSB :: Socket -> BSB.Builder -> IO ()
sendBSB s bs = Net.sendLazy s (BSB.toLazyByteString bs)

encodeFieldList :: [Field] -> BSB.Builder
encodeFieldList xs =
    repeatedlyEncode (\x -> encodeField x <> encodeLineEnd) xs 
    <> encodeLineEnd

-- Exercise 30 - Tripping at the finish line

{-
1. Include too few line breaks:
    - in fileStreaming comment out: sendBSB s (encodeFieldList [])
    - run curl in verbose mode: curl --http1.1 --verbose http://localhost:8000
    - after connecting and streaming the file, curl reports:

        * no chunk, no close, no size. Assume close to signal end
        <
        0
        * Closing connection 0

2. Include too many line breaks: 
    - in fileStreaming add to the end: sendBSB s encodeLineEnd
    - run curl in verbose mode: curl --http1.1 --verbose http://localhost:8000
    - after connecting and streaming the file, curl reports:

        * no chunk, no close, no size. Assume close to signal end
        <
        0

        * Closing connection 0
For 1, because we're missing a line break, the server closes the connection after printing the last
chunk (chunk of size 0). For 2, because there's an extra line break, the server prints a line break 
after the last chunk.
-}

-- Exercise 32 - Infinite response

-- Infinite stream of numbers counting form one
infiniteCountServer :: IO ()
infiniteCountServer = serve @IO HostAny "8000" \(s, _) -> do
    sendBSB s (encodeStatusLine (status ok))
    sendBSB s (encodeFieldList [transferEncodingChunked])
    for_ [1 ..] \n -> do 
        sendOneOfInfinity s n
        threadDelay 500000

sendOneOfInfinity :: Socket -> Natural -> IO ()
sendOneOfInfinity s n = 
    sendBSB s $ encodeChunk $ dataChunk $ ChunkData $ A.showIntegralDecimal n <> A.fromCharList [A.LineFeed]

-- Infinite stream of UTCTimes
infiniteTimeServer :: IO ()
infiniteTimeServer = serve @IO HostAny "8000" \(s, _) -> do
    sendBSB s (encodeStatusLine (status ok))
    sendBSB s (encodeFieldList [transferEncodingChunked])
    forever do
        sendOneTimeOfInfinity s
        threadDelay 500000

sendOneTimeOfInfinity :: Socket -> IO ()
sendOneTimeOfInfinity s = do
    now <- Time.getCurrentTime
    sendBSB s $ encodeChunk $ dataChunk $ ChunkData $ show now <> A.fromCharList [A.LineFeed]

--
-- Chapter 12
--

-- Producer Example

demoProducer :: Producer Text IO ()
demoProducer = do
    yield (T.pack "one, ")
    yield (T.pack "two, ")
    yield (T.pack "three")
    replicateM_ 10 do
        liftIO (threadDelay 100000)
        yield (T.pack ".")
    yield (T.pack " GO!!!")

putTextConsumer :: Consumer Text IO ()
putTextConsumer =
    forever do
        x <- await
        putText x

-- To run the demo in GHCI:
--     > runEffect (demoProducer >-> putTextConsumer)

-- Data types for streaming response

data MaxChunkSize = MaxChunkSize Int

data StreamingResponse = StreamingResponse StatusLine [Field] (Maybe ChunkedBody)

data ChunkedBody = ChunkedBody (Producer Chunk IO ())

fileStreaming2 = do
    dir <- getDataDir
    serve @IO HostAny "8000" \(s, _) -> runResourceT @IO do
        (_, h) <- binaryFileResource (dir </> "stream.txt") ReadMode
        let r = hStreamingResponse h (MaxChunkSize 1024)
        liftIO (sendStreamingResponse s r)

hStreamingResponse :: Handle -> MaxChunkSize -> StreamingResponse
hStreamingResponse h maxChunkSize = StreamingResponse statusLine fields (Just body)
    where
        statusLine = status ok
        fields = [transferEncodingChunked]
        body = chunkedBody (hChunks h maxChunkSize)

hChunks :: Handle -> MaxChunkSize -> Producer ByteString IO ()
hChunks h (MaxChunkSize mcs) = proceed
    where 
        proceed = do
            chunk <- liftIO (BS.hGetSome h mcs)
            case BS.null chunk of
                True -> return ()
                False -> do
                    yield chunk
                    proceed

stringsToChunks :: Pipe ByteString Chunk IO ()
stringsToChunks =
    forever do
        bs <- await
        yield (dataChunk (ChunkData bs))

chunkedBody :: Producer ByteString IO () -> ChunkedBody
chunkedBody xs = ChunkedBody (xs >-> stringsToChunks)

build :: Pipe BSB.Builder ByteString IO ()
build = forever do
    bsb <- await
    let chunks = LBS.toChunks (BSB.toLazyByteString bsb)
    for_ chunks \x ->
        yield x

encodeChunks :: Pipe Chunk BSB.Builder IO ()
encodeChunks =
    forever do
        chunk <- await
        yield (encodeChunk chunk)

encodeStreamingResponse (StreamingResponse statusLine headers bodyMaybe) =
    do
        yield (encodeStatusLine statusLine)
        yield (encodeFieldList headers)

        for_ bodyMaybe \(ChunkedBody body) -> do
            body >-> encodeChunks 
            yield encodeLastChunk
            yield (encodeFieldList [])      -- Trailer fields

        >-> build

sendStreamingResponse :: Socket -> StreamingResponse -> IO ()
sendStreamingResponse s r = runEffect @IO (encodeStreamingResponse r >-> toSocket s)

-- Exercise 32 - Into the socket

toSocket :: Socket -> Consumer ByteString IO ()
toSocket s = 
    forever do
        x <- await
        liftIO $ Net.send s x

-- Exercise 33 - Produce until

produceUntil :: IO chunk -> (chunk -> Bool) -> Producer chunk IO ()
produceUntil getChunk isEnd = proceed
    where
        proceed = do
            chunk <- liftIO getChunk
            case isEnd chunk of
                True -> return ()
                False -> do
                    yield chunk
                    proceed

hChunks' :: Handle -> MaxChunkSize -> Producer ByteString IO ()
hChunks' h (MaxChunkSize mcs) = 
    produceUntil (BS.hGetSome h mcs) BS.null

-- To test: replace hChunks with hChunks' in hStreamingResponse

-- Exercise 34 - File copying

copyGreetingStream = runResourceT @IO do
    dir <- liftIO getDataDir
    (_, h1) <- binaryFileResource (dir </> "greeting.txt") ReadMode
    (_, h2) <- binaryFileResource (dir </> "greeting1.txt") WriteMode
    liftIO $ hCopy h1 h2

hCopy :: Handle -> Handle -> IO ()
hCopy source destination = runEffect @IO (producer >-> consumer)
    where
        producer = hChunks' source (MaxChunkSize 1024)
        consumer = 
            forever do
                chunk <- await
                liftIO $ BS.hPutStr destination chunk

-- Exercise 35 - Copying to multiple destinations

fileCopyMany :: FilePath -> [FilePath] -> IO ()
fileCopyMany source destinations = runResourceT @IO do
    (_, hSource) <- binaryFileResource source ReadMode
    hDestinations <- forM destinations \fp -> do
        (_, h) <- binaryFileResource fp WriteMode
        return h
    liftIO $ hCopyMany hSource hDestinations

hCopyMany :: Handle -> [Handle] -> IO ()
hCopyMany source destinations = runEffect @IO (producer >-> consumer)
    where
        producer = hChunks' source (MaxChunkSize 1024)
        consumer = 
            forever do
                chunk <- await
                forM_ destinations \dest -> (liftIO $ BS.hPutStr dest chunk)
        

demoFileCopyMany = runResourceT @IO do
    dir <- liftIO getDataDir
    let src = dir </> "greeting.txt"
    let dests = map (\n -> dir </> ("greeting" <> show n <> ".txt")) [1..3]
    liftIO $ fileCopyMany src dests

 --
 -- Chapter 13
 --

data ResourceName = ResourceName Text deriving (Eq, Ord, Show)

data ResourceMap = ResourceMap (Map ResourceName FilePath) deriving Show

resourceMap :: FilePath -> ResourceMap
resourceMap dir = ResourceMap $ Map.fromList 
    [ (streamResource, dir </> "stream.txt") ,
      (readResource, dir </> "read.txt")]

streamResource = ResourceName (T.pack "/stream")

readResource = ResourceName (T.pack "/read")


-- Attoparsec examples

countString ::ByteString
countString = [A.string|one-two-three-four|]

{-
Examples:

P.parseOnly (P.takeWhile A.isDigit) countString
> Right ""

P.parseOnly (P.takeWhile A.isLetter) countString
> Right "one"

P.parseOnly (P.takeWhile1 A.isLetter) countString
> Right "one

P.parseOnly (P.takeWhile1 A.isDigit) countString
> Left "Failed reading: takeWhile1"

-}

takeTwoWords :: Parser (ByteString, ByteString)
takeTwoWords = do
    a <- P.takeWhile A.isLetter
    _ <- P.string (A.fromCharList [A.HyphenMinus])
    b <- P.takeWhile A.isLetter
    return (a, b)


-- HTTP Request line parsing

{-
request-line from RFC 9112:

request-line - method SP request-target SP HTTP-version CRLF

method = token
token = 1*tchar (tchar is letter, number, or one of ! # $ % & ' * + - . ^ _ ` | ~)

HTTP-version  = HTTP-name "/" DIGIT "." DIGIT
-}
    
spaceParser :: Parser ByteString
spaceParser = P.string (A.fromCharList [A.Space])

lineEndParser :: Parser ByteString
lineEndParser = P.string (A.fromCharList crlf)

requestLineParser :: Parser RequestLine
requestLineParser = do
    method  <- methodParser             <?> "Method" 
    _       <- spaceParser              <|> fail "Method should be followed by a space"
    target  <- requestTargetParser      <?> "Target"
    _       <- spaceParser              <|> fail "Target should be followed by a space"
    version <- versionParser            <?> "Version"
    _       <- lineEndParser            <|> fail "Version should be followed by end of line"
    return (RequestLine method target version)

methodParser :: Parser Method
methodParser = do
    x <- tokenParser 
    return (Method x)

tokenParser :: Parser (A.ASCII ByteString)
tokenParser = do
    bs <- P.takeWhile1 isTchar
    case A.convertStringMaybe bs of
        Just asciiBS -> return asciiBS
        Nothing -> fail "Non-ASCII tchar"

isTchar :: Word8 -> Bool
isTchar c = c `elem` tcharSymbols || A.isDigit c || A.isLetter c

tcharSymbols :: [Word8]
tcharSymbols = A.fromCharList [A.ExclamationMark, A.NumberSign, A.DollarSign,
    A.PercentSign, A.Ampersand, A.Apostrophe, A.Asterisk, A.PlusSign, A.HyphenMinus,
    A.FullStop, A.Caret, A.Underscore, A.GraveAccent, A.VerticalLine, A.Tilde]

-- RFC 9112 describes several forms that the request target can take and 
-- references RFC 3986 for URI syntax. We'll sumplify and allow a request
-- to be a nonempty string of visible ASCII characters (VCHAR)
requestTargetParser :: Parser RequestTarget
requestTargetParser = do
    bs <- P.takeWhile1 A.isVisible
    case A.convertStringMaybe bs of
        Just asciiBS -> return (RequestTarget asciiBS)
        Nothing -> fail "Non-ASCII vchar"

-- Exercise 38 - Better parse errors
-- Added error context/descriptions to versionParser
versionParser :: Parser Version
versionParser = do
    _ <- P.string [A.string|HTTP|]
    _ <- P.string (A.fromCharList [A.Slash])        <|> fail "HTTP should be followed by a slash"
    x <- digitParser                                <?> "First digit"
    _ <- P.string (A.fromCharList [A.FullStop])     <|> fail "Missing '.' after first digit"
    y <- digitParser                                <?> "Second digit"
    return (Version x y)

digitParser :: Parser Digit
digitParser = do
    x <- P.anyWord8
    case A.isDigit x of
        True -> return (A.word8ToDigitUnsafe x)
        False -> fail "0-9 expected"

-- Parse a stream from a socket

data Input = Input (P.RestorableInput IO ByteString)

parseFromSocket :: Socket -> MaxChunkSize -> IO Input
parseFromSocket s (MaxChunkSize mcs) = do
    i <- P.newRestorableIO (S.recv s mcs)
    return (Input i)

readRequestLine :: Input -> IO RequestLine
readRequestLine (Input i) = do
    result <- P.parseAndRestore i (requestLineParser <?> "Request line")
    case result of
        Left parseError -> fail (P.showParseError parseError)
        Right requestLine -> return requestLine

resourceServer :: IO ()
resourceServer = do
    dir <- getDataDir
    let resources = resourceMap dir
    let maxChunkSize = MaxChunkSize 1024
    serve @IO HostAny "8000" \(s, _) -> serveResourceOnce resources maxChunkSize s

serveResourceOnce :: ResourceMap -> MaxChunkSize  -> Socket -> IO ()
serveResourceOnce resources maxChunkSize s = runResourceT @IO do
    i <- liftIO $ parseFromSocket s maxChunkSize
    RequestLine _ target _ <- liftIO (readRequestLine i)
    filePath <- liftIO (getTargetFilePath resources target)
    (_, h) <- binaryFileResource filePath ReadMode
    let r = hStreamingResponse h maxChunkSize
    liftIO (sendStreamingResponse s r)

getTargetFilePath :: ResourceMap -> RequestTarget -> IO FilePath
getTargetFilePath rs target =
    case targetFilePathMaybe rs target of
        Nothing -> fail "not found"
        Just fp -> return fp

targetFilePathMaybe :: ResourceMap -> RequestTarget -> Maybe FilePath
targetFilePathMaybe (ResourceMap rs) (RequestTarget target) = 
    Map.lookup r rs
    where
        r = ResourceName (A.asciiByteStringToText target)

-- Exercise 36 - Parsing parentheses
parenParser :: Parser ByteString
parenParser = do
    _ <- P.string (A.fromCharList [A.LeftParenthesis])
    x <- P.takeWhile (/= A.fromChar A.RightParenthesis)
    _ <- P.string (A.fromCharList [A.RightParenthesis])
    return x

-- Exercise 37 - To digit, maybe
digitParser' :: Parser Digit
digitParser' = do
    x <- P.anyWord8
    case A.word8ToDigitMaybe x of
        Just asciiDigit -> return asciiDigit
        Nothing -> fail "0-9 expected"

-- Exercise 39 - Status line

{-
status-line from RFC 9112:

status-line = HTTP-version SP status-code SP [ reason-phrase ]

status-code = 3DIGIT

reason-phrase  = 1*( HTAB / SP / VCHAR / obs-text )

-}

-- Parse status line
statusLineParser :: Parser StatusLine
statusLineParser = do
    version     <- versionParser            <?> "Version"
    _           <- spaceParser              <|> fail "Version should be followed by a space"
    statusCode  <- statusCodeParser         <?> "Status Code"
    _           <- spaceParser              <|> fail "Status Code should be followed by a space"
    reason      <- reasonParserMaybe        <?> "Reason Phrase"
    _           <- lineEndParser
    return (StatusLine version statusCode reason)

statusCodeParser :: Parser StatusCode
statusCodeParser = do
    d1 <- digitParser'
    d2 <- digitParser'
    d3 <- digitParser'
    return (StatusCode d1 d2 d3)

reasonParserMaybe :: Parser (Maybe ReasonPhrase)
reasonParserMaybe = 
    Just <$> reasonParser <|>  pure Nothing

reasonParser :: Parser ReasonPhrase
reasonParser = do
    bs <- P.takeWhile1 isReasonChar
    case A.convertStringMaybe bs of
        Just asciiBS -> return (ReasonPhrase asciiBS)
        Nothing -> fail "Non-ASCII vchar"

isReasonChar :: Word8 -> Bool
isReasonChar c = A.isVisible c || c `elem` (fmap A.fromChar [A.Space, A.HorizontalTab])

-- Test parsing
statusLineParseTest :: StatusLine -> Maybe String
statusLineParseTest s = 
    case result of
        Right s' | s /= s' -> Just (show s') 
        Right _ ->  Nothing
        Left e -> Just e
    where
        encodedStatus = LBS.toStrict $ BSB.toLazyByteString $ encodeStatusLine s
        result = P.parseOnly (statusLineParser <* P.endOfInput) encodedStatus

-- Exercise 40 - P.string

stringParser1 :: ByteString -> Parser ByteString
stringParser1 bs = do
    case BS.uncons bs of
        Nothing -> return bs
        Just (b, bs') -> do
            x <- P.anyWord8
            unless (b == x) (fail "string") 
            res <- stringParser1 bs'
            return $ BS.cons b res

stringParser2 :: ByteString -> Parser ByteString
stringParser2 bs = do
    xs <- replicateM (BS.length bs) P.anyWord8
    when (BS.pack xs /= bs) (fail "string")
    return bs


--
-- Chapter 14
--

-- Status Codes

-- 400
badRequest = Status
    (StatusCode Digit4 Digit0 Digit0)
    (Just (ReasonPhrase [A.string|Bad request|]))

-- 404
notFound = Status
    (StatusCode Digit4 Digit0 Digit4)
    (Just (ReasonPhrase [A.string|Not found|]))

-- 405
methodNotAllowed = Status
    (StatusCode Digit4 Digit0 Digit5)
    (Just (ReasonPhrase [A.string|Method not allowed|]))

-- 500
serverError = Status
    (StatusCode Digit5 Digit0 Digit0)
    (Just (ReasonPhrase [A.string|Server error|]))

-- 505
versionNotSupported = Status
    (StatusCode Digit5 Digit0 Digit5) 
    (Just (ReasonPhrase [A.string|Version not supported|]))

-- Construct Error Responses

textResponse :: Status -> [Field] -> LText -> Response
textResponse s additionalFields bodyText =
    Response (status s)
             ([typ, len] <> additionalFields)
             (Just (Body (LT.encodeUtf8 bodyText)))
    where
        typ = Field contentType plainUtf8
        len = Field contentLength (bodyLengthValue body)
        body = Body (LT.encodeUtf8 bodyText)


-- Logging Errors

data LogEvent = LogEvent LText

printLogEvent :: LogEvent -> IO ()
printLogEvent (LogEvent x) = LT.putStrLn x

data Error = Error (Maybe Response) [LogEvent]

handleRequestError :: (LogEvent -> IO b) -> Socket -> Error -> IO ()
handleRequestError log s (Error responseMaybe events) = do
    for_ events log
    for_ responseMaybe (sendResponse s)

-- Error Responses

-- Malformed request
requestParseError :: P.ParseError -> Error
requestParseError parseError = Error (Just response) [event]
    where
        response = textResponse badRequest [] message
        event = LogEvent message
        message = TB.toLazyText (
            TB.fromString "Malformed request: " <>
            TB.fromString (P.showParseError parseError))

-- Resource doesn't exist
notFoundError :: Error
notFoundError = Error (Just response) []
    where
        response = textResponse notFound [] message
        message = LT.pack "It just isn't there."

-- Method not supported
-- RC 9110 requires an Allow field in the Response which lists the allowed methods
-- No need to send a message body
methodError :: [Method] -> Error
methodError supportedMethods = Error (Just response) []
    where
        response = textResponse methodNotAllowed [allowField supportedMethods] LT.empty

allowField :: [Method] -> Field
allowField methods = Field (FieldName [A.string|Allow|]) value
    where
        value = FieldValue $ commaList $ map (\(Method m) -> m) methods
        commaList xs = fold $ intersperse (A.fromCharList [A.Comma, A.Space]) xs

-- Can't open file
fileOpenError :: FilePath -> SomeException -> Error
fileOpenError filePath ex = Error (Just response) [event]
    where
        response = textResponse serverError [] (LT.pack "Something went wrong.")
        event = LogEvent $ TB.toLazyText $
            TB.fromString "Failed to open file " <>
            TB.fromString (show filePath) <> TB.fromString ": " <>
            TB.fromString (displayException ex)

-- Exceptions after status has been sent
ungracefulError :: SomeException -> Error
ungracefulError ex = Error Nothing [event]
    where
        event = LogEvent (LT.pack (displayException ex))

-- Wrapper to catch any IO exception
handleIOExceptions :: IO (Either Error a) -> IO (Either Error a)
handleIOExceptions action = do
    result <- tryAny action
    case result of
        Left e -> return (Left (ungracefulError e))
        Right x -> return x


-- Rewritten resourceServer to allow thread-safe logging
-- Rewritten functions will have an X suffix

getTargetFilePathX :: ResourceMap -> RequestTarget -> Either Error FilePath
getTargetFilePathX rs t =
    case targetFilePathMaybe rs t of
        Nothing -> Left notFoundError
        Just fp -> Right fp

readRequestPart :: Input -> String -> Parser a -> IO (Either Error a)
readRequestPart (Input i) description p = do
    result <- liftIO (P.parseAndRestore i (p <?> description))
    case result of
        Left e -> return $ Left $ requestParseError e
        Right requestLine -> return (Right requestLine)

readRequestLineX :: Input -> IO (Either Error RequestLine)
readRequestLineX i = readRequestPart i "Request line" requestLineParser

binaryFileResourceX :: FilePath -> IOMode -> ResourceT IO (Either Error (ReleaseKey, Handle))
binaryFileResourceX fp mode = do
    result <- tryAny (binaryFileResource fp mode)
    case result of
        Left e -> return (Left (fileOpenError fp e))
        Right x -> return (Right x)

requireMethodX :: [Method] -> Method -> Either Error ()
requireMethodX supportedMethods x =
    case (elem x supportedMethods) of
        False -> Left (methodError supportedMethods)
        True -> Right ()

sendStreamingResponseX :: Socket -> StreamingResponse -> IO (Either Error ())
sendStreamingResponseX s r = do
    result <- tryAny $ runEffect @IO (encodeStreamingResponse r >-> toSocket s)
    case result of
        Left e -> return (Left (ungracefulError e))
        Right x -> return (Right x)

serveResourceOnceX :: ResourceMap -> MaxChunkSize -> Socket -> IO (Either Error ())
serveResourceOnceX resources maxChunkSize s =
    handleIOExceptions $ runResourceT @IO $
    runExceptT @Error @(ResourceT IO) do
        i <- liftIO $ parseFromSocket s maxChunkSize
        RequestLine method target version <- ExceptT $ liftIO $ readRequestLineX i
        ExceptT $ return (requireMethodX allowedMethods method)
        ExceptT $ return (requireVersionX supportedVersion version)
        fp <- ExceptT $ return (getTargetFilePathX resources target)
        (_, h) <- ExceptT $ binaryFileResourceX fp ReadMode
        let r = hStreamingResponse h maxChunkSize
        ExceptT $ liftIO $ sendStreamingResponseX s r
    where
        allowedMethods = [ Method [A.string|GET|] ]
        supportedVersion = http_1_1

-- Exercise 41 - ResourceServerX

resourceServerX = do
    dir <- getDataDir
    let resources = resourceMap dir
    let maxChunkSize = MaxChunkSize 1024
    unforkAsyncIO_ printLogEvent \log ->
        serve @IO HostAny "8000" \(s, _) -> do
            result <- serveResourceOnceX resources maxChunkSize s
            case result of
                Left e -> handleRequestError log s e 
                Right _ -> return ()

-- Exercise 42 - Check method and version

-- Method check: added requireMethodX to serveResourceOnceX

-- Version check 
-- requireVersionX added to serveResourceOnceX
versionError :: Error
versionError = Error (Just response) []
    where
        response = textResponse versionNotSupported [] message
        message = LT.pack "Only HTTP 1.1 is supported."

requireVersionX :: Version -> Version -> Either Error ()
requireVersionX supportedVersion requestVersion =
    case (requestVersion == supportedVersion) of
        False -> Left versionError
        True -> Right ()
    
-- Testing errors
--
-- Unavailable resource -> message returned to client
--   curl --http1.1 http://localhost:8000
-- Unable to open file (change permissions) -> log message generated
--   chmod 0 ~/.local/share/sockets-and-pipes/read.txt
--   curl --http1.1 http://localhost:8000/read
-- Unsupported method -> response (in verbose output)
--   curl --http1.1 --verbose -X POST http://localhost:8000
-- Unsupported version
--   Test request of version not HTTP 1.1:
streamRequestString1 =
    line [A.string|GET /stream HTTP/0.1|] <>
    line [A.string|User-Agent: curl/7.64.1|] <>
    line [A.string|Accept-Language: en, mi|] <>
    line [A.string||]
--  Test with: > testHttpRequest "127.0.0.1" "8000" streamRequestString1

-- Exercise 43

-- Malformed request -> response and log generated
--   Test request with two spaces after method:
streamRequestString2 =
    line [A.string|GET  /stream HTTP/1.1|] <>
    line [A.string|User-Agent: curl/7.64.1|] <>
    line [A.string|Accept-Language: en, mi|] <>
    line [A.string||]
--  Test with: > testHttpRequest "127.0.0.1" "8000" streamRequestString2





