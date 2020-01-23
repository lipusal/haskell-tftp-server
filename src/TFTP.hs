module TFTP where

import Data.Word
import Data.Char
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import Debug.Trace
import System.IO hiding (hGetContents, hPut)
import System.IO.Error
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, recvFrom, send, sendAll, sendAllTo)
import Data.Maybe
import Data.Either

-- TFTP DATA packet size is 512 bytes, minus 2 bytes for opcode, minus 2 bytes for block number
blockSize = 508

data Session = Session {
    sock :: Socket, -- Socket with local and remote TIDs (ports)
    openingPacket :: Packet,
    blockNum :: Integer,
    pendingPackets :: [Packet]
} deriving Show

data Packet =
    RRQ String String -- opcode 1 + filename + 0 + mode + 0
    | WRQ String String -- opcode 2 + filename + 0 + mode + 0
    | DATA Word16 BS.ByteString -- opcode 3 + block number + data TODO also consider lazy bytestrings
    | DATA2 Word16 [Word8] -- opcode 3 + block number + data TODO also consider strict/lazy bytestrings
    | ACK Word16 -- opcode 4 + block number
    | ERROR Word16 String deriving Show -- opcode 5 + error code + error message + 0

newSession :: Socket -> Packet -> Session
newSession sock packet = Session { sock = sock, openingPacket = packet, blockNum = 0, pendingPackets = [] }

handle :: Session -> IO ()
handle session@(Session { openingPacket = (RRQ filename mode) }) = do
    packets <- fileToPackets filename (map toLower mode)
    sendFile packets session
handle session@(Session { openingPacket = (WRQ filename mode) }) = receiveFile session
handle sess = trace ("Attempting to handle session with invalid opening packet: " ++ show sess) return ()

receiveFile :: Session -> IO ()
receiveFile session@(Session { openingPacket = (WRQ filename mode) }) = do
    let socket = sock session
    eitherHandle <- openFileHandler filename WriteMode mode
    either
        (\errPacket -> sendAll socket (toByteString errPacket))
        (\handle -> do
            sendPacket socket (ACK 0)
            file <- recvFile socket 1
            BS.hPut handle file
            hClose handle
        )
        eitherHandle
receiveFile _ = return ()

sendPacket :: Socket -> Packet -> IO()
sendPacket sock packet = trace(">>>> " ++ show packet) sendAll sock (toByteString packet)

recvPacket :: Socket -> IO(Maybe Packet)
recvPacket socket = do
    recvData <- recv socket blockSize
    let result = fromByteString recvData
    trace("<<<< " ++ show result) return result

sendFile :: [Packet] -> Session -> IO ()
sendFile filePackets session = mapM_ (sendDataPacket session) filePackets

-- Send data packet until appropriate ACK is received
sendDataPacket :: Session -> Packet -> IO ()
sendDataPacket session packet@(DATA blockNum payload) = do
    sendPacket (sock session) packet    
    response <- recvPacket (sock session)
    if not(isAck blockNum response) then sendDataPacket session packet else return () -- TODO better handle incorrect packages. Send error and close connection

recvFile :: Socket -> Word16 -> IO(BS.ByteString)
recvFile sock blockNum = do
    (packet, nextBlockNum) <- recvDataPacket sock blockNum
    sendPacket sock (ACK blockNum)
    let payload = dataPayload packet
    if (isNothing nextBlockNum) then return payload else do
        remainder <- recvFile sock (fromJust nextBlockNum)
        return(BS.concat [payload, remainder])

recvDataPacket :: Socket -> Word16 -> IO(Packet, Maybe Word16)
recvDataPacket sock expectedBlockNum = do
    packet <- recvPacket sock
    if (isData expectedBlockNum packet)
        then return(fromJust packet, nextDataBlockNum(fromJust packet))
        else recvDataPacket sock expectedBlockNum -- retry until successful

-- Checks if the given package is a DATA package with the specified block number
isData :: Word16 -> Maybe Packet -> Bool
isData expectedBlockNum (Just(DATA blockNum _)) = blockNum == expectedBlockNum
isData _ _ = False

dataLength :: Packet -> Int
dataLength (DATA _ payload) = BS.length payload

nextDataBlockNum :: Packet -> Maybe Word16
nextDataBlockNum packet@(DATA blockNum payload) = trace("DATA packet length = " ++ show (dataLength packet) ++ ", expected = " ++ show blockSize) (if dataLength packet == blockSize then Just(blockNum+1) else Nothing)

dataPayload :: Packet -> BS.ByteString
dataPayload (DATA _ payload) = payload
 
-- Checks if given package is an ACK for the specified block number
isAck :: Word16 -> Maybe Packet -> Bool
isAck expectedBlockNum (Just(ACK blocknum)) = blocknum == expectedBlockNum
isAck _ _ = False

fromByteString :: BS.ByteString -> Maybe Packet
fromByteString bs = fromOpcode opcode payload
    where (header,payload) = BS.splitAt 2 bs
          opcode = byteStringToWord16 header

fromOpcode :: Word16 -> BS.ByteString -> Maybe Packet
fromOpcode 1 d = Just (RRQ filename mode)
    where (filename:mode:_) = map byteStringtoString (BS.split 0 d)
fromOpcode 2 d = Just (WRQ filename mode)
    where (filename:mode:_) = map byteStringtoString (BS.split 0 d)
fromOpcode 3 d = Just (DATA blockNum payload)
    where (header,payload) = BS.splitAt 2 d
          blockNum = byteStringToWord16 header
fromOpcode 4 d = Just (ACK (byteStringToWord16 d))
fromOpcode 5 d = Just (ERROR errCode errMsg)
    where (header,errBytes) = BS.splitAt 2 d
          errCode = byteStringToWord16 header
          errMsg = byteStringtoString (BS.init errBytes) -- init to remove the trailing 0
fromOpcode x y = trace ("Received opcode=" ++ show x ++ " and data=" ++ byteStringtoString y) Nothing

toByteString :: Packet -> BS.ByteString
-- TODO check out Data.ByteString.Builder
toByteString (RRQ filename mode) = BS.concat [
    word16ToByteString 1,
    stringToByteString filename,
    BS.singleton 0,
    stringToByteString mode,
    BS.singleton 0]
toByteString (WRQ filename mode) = BS.concat [
    word16ToByteString 2,
    stringToByteString filename,
    BS.singleton 0,
    stringToByteString mode,
    BS.singleton 0]
toByteString (DATA blocknum payload) = BS.concat [
    word16ToByteString 3,
    word16ToByteString blocknum,
    payload]
toByteString (ACK blocknum) = BS.concat [
    word16ToByteString 4,
    word16ToByteString blocknum]
toByteString (ERROR errNum errMsg) = BS.concat [
    word16ToByteString 5,
    word16ToByteString errNum,
    stringToByteString errMsg,
    BS.singleton 0]


process :: Packet -> IO ([Packet])
process (RRQ filename mode) = fileToPackets filename (map toLower mode)
process (WRQ filename mode) = return [ACK 0] -- TODO: We have to save state to know what we're reading from. Use TIDs
process (DATA blockNum payload) = return [ACK blockNum] -- TODO: Write to file, we have to have a session set to know file name
process (ACK blockNum) = return [] -- TODO: Send more data on RRQ, or close connection, etc.
process (ERROR errNum errMsg) = trace ("ERROR " ++ show errNum ++ ": " ++ errMsg) return []

fileToPackets :: String -> String -> IO ([Packet])
fileToPackets filename mode = do
    eitherHandle <- openFileHandler filename ReadMode mode
    either
        (\errorPacket -> return [errorPacket])
        (\handle -> do
            contents <- BS.hGetContents handle
            hClose handle
            return (mapWithIndex (\dat index -> DATA (fromIntegral (index+1)) dat) (chunks contents))
        )
        eitherHandle

openFileHandler :: FilePath -> IOMode -> String -> IO (Either Packet Handle)
openFileHandler path mode "netascii" = (do
    handle <- openFile path mode
    return(Right handle)) `catchIOError` (\e -> return(Left(myHandler e)))
openFileHandler path mode "octet" = (do
        handle <- openBinaryFile path mode
        return(Right handle)) `catchIOError` (\e -> return(Left(myHandler e)))
openFileHandler _ _ tftpMode = return(Left(ERROR 4 ("Invalid mode " ++ tftpMode)))

myHandler :: IOError -> Packet -- TODO rename
myHandler e
    | isAlreadyInUseError e = ERROR 2 "File already in use"
    | isDoesNotExistError e = ERROR 1 "File not found"
    | isPermissionError e = ERROR 2 ("Permission error: " ++ ioeGetErrorString e)
    | otherwise = ERROR 2 ("Error: " ++ ioeGetErrorString e)

-- Split a bytestring into chunks of size blockSize
chunks :: BS.ByteString -> [BS.ByteString]
chunks b = if BS.null b then [] else chunk:(chunks remainder)
    where (chunk, remainder) = BS.splitAt blockSize b

-- Source: https://stackoverflow.com/a/16192050/2333689
mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = zipWith f l [0..]


fromByteString2 :: BS.ByteString -> Maybe Packet
fromByteString2 d = fromCharArray2 opcode16 rest
    where (opHigh:opLow:rest) = BS.unpack d
          opcode16 = word8to16 [opHigh,opLow]

fromCharArray2 :: Word16 -> [Word8] -> Maybe Packet
fromCharArray2 1 d = Just (RRQ filename mode)
    where (filename, d1) = extractString d
          (mode, _) = extractString d1
fromCharArray2 2 d = Just (WRQ filename mode)
    where (filename, d1) = extractString d
          (mode, _) = extractString d1
fromCharArray2 3 (b1:b2:b) = Just (DATA2 (word8to16 [b1,b2]) b)
fromCharArray2 4 (b1:b2:_) = Just (ACK (word8to16 [b1,b2]))
fromCharArray2 5 (b1:b2:b) = Just (ERROR (word8to16 [b1,b2]) errMsg)
    where (errMsg, _) = extractString b
fromCharArray2 x y = trace ("Received opcode=" ++ show x ++ " and data=" ++ (showList y "")) Nothing


byteStringtoString :: BS.ByteString -> String
byteStringtoString = map (chr.fromEnum).BS.unpack

stringToByteString :: String -> BS.ByteString
stringToByteString str = BS.pack (map (toEnum.ord) str)

singleWord8to16 :: Word8 -> Word16
singleWord8to16 = fromInteger.toInteger

-- Convert the first two bytes of the given bytestring to a 2-byte integer
byteStringToWord16 :: BS.ByteString -> Word16
-- return ((higher << 8) | lower) :: Word16
byteStringToWord16 bs = fromIntegral ((.|.) (high `shiftL` 8) (low))
    where (high:low:_) = map fromEnum (BS.unpack bs)
        --   (high2:low2:_) = (BS.unpack bs)
        --   high = fromIntegral(fromEnum high2)
        --   low = fromIntegral(fromEnum low2)

word8to16 :: [Word8] -> Word16
-- return (higher << 8) | lower
word8to16 (higher:lower:_) = (.|.) (fromInteger(toInteger higher) `shiftL` 8) (fromInteger(toInteger lower))

-- Extract characters until a 0, and return the formed string plus the remainder of the char array
extractString :: [Word8] -> (String, [Word8])
extractString [] = ("", [])
extractString (head:rest) = if head == 0 then ("", rest) else (show head ++ substring, remainder)
    where (substring, remainder) = extractString rest

word16ToByteString :: Word16 -> BS.ByteString
-- return [num >> 8, num & 255]
word16ToByteString num = BS.pack [higher,lower]
    where higher = fromInteger((toInteger num) `shiftR` 8)
          lower = fromInteger(toInteger ((.&.) num 255))
