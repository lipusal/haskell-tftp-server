module TFTP where

import Data.Word
import Data.Char
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import TFTP.Packet
import TFTP.Files
import TFTP.Constants
import Debug.Trace
import System.IO hiding (hGetContents, hPut)
import System.IO.Error
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, recvFrom, send, sendAll, sendAllTo)
import Data.Maybe
import Data.Either
import Netascii
import Control.Monad

handle :: BS.ByteString -> Socket -> IO ()
handle payload socket = do
    let initialPacket = fromByteString payload
    putStrLn("<<<< " ++ show initialPacket)
    if isNothing initialPacket
        then sendPacket socket (ERROR 4 "Not a TFTP packet")
        else handleInitialPacket (fromJust initialPacket) socket

-- PRIVATE

handleInitialPacket :: Packet -> Socket -> IO()
handleInitialPacket packet@(RRQ filename mode) socket = handleRRQ packet socket
handleInitialPacket packet@(WRQ filename mode) socket = handleWRQ packet socket
handleInitialPacket _ socket = sendPacket socket (ERROR 4 "Not a valid initial TFTP packet")

handleRRQ :: Packet -> Socket -> IO()
handleRRQ (RRQ filename mode) socket = do
    eitherPackets <- fileToPackets filename (map toLower mode)
    either
        (\errPacket -> sendPacket socket errPacket)
        (\filePackets -> sendFile filePackets socket)
        eitherPackets

handleWRQ :: Packet -> Socket -> IO()
handleWRQ packet@(WRQ filename mode) socket = receiveFile packet socket

receiveFile :: Packet -> Socket -> IO ()
receiveFile (WRQ filename mode) socket = do
    eitherHandle <- openFileHandler filename WriteMode mode
    either
        (sendPacket socket)  -- Partial application, this receives the error packet
        (\handle -> do
            sendPacket socket (ACK 0)
            maybeFileContents <- recvFile mode socket 1
            when (isJust maybeFileContents) (BS.hPut handle (fromJust maybeFileContents))
            hClose handle
        )
        eitherHandle

sendPacket :: Socket -> Packet -> IO()
sendPacket sock packet = trace(">>>> " ++ show packet) sendAll sock (toByteString packet)

recvPacket :: Socket -> IO(Maybe Packet)
recvPacket socket = do
    recvData <- recv socket packetSize
    let result = fromByteString recvData
    trace("<<<< " ++ show result) return result

sendFile :: [Packet] -> Socket -> IO ()
sendFile filePackets socket = mapM_ (sendDataPacket socket) filePackets

-- Send data packet until appropriate ACK is received
sendDataPacket :: Socket -> Packet -> IO ()
sendDataPacket socket packet@(DATA blockNum payload) = do
    sendPacket socket packet
    response <- recvPacket socket
    when (not $ isAck blockNum response) (sendDataPacket socket packet) -- TODO better handle incorrect packages. Send error and close connection

recvFile :: String -> Socket -> Word16 -> IO(Maybe BS.ByteString)
recvFile mode sock blockNum = do
    (packet, nextBlockNum) <- recvDataPacket sock blockNum
    if mode == "netascii" && (not $ isNetascii(dataPayload packet))
        then (do
            sendPacket sock (ERROR 0 "File contents are outside netascii range, use octet mode")
            return Nothing)
        else (do 
            sendPacket sock (ACK blockNum)
            let payload = dataPayload packet
            maybe
                (return $ Just payload)
                (\nextBlock -> do
                    remainder <- recvFile mode sock nextBlock
                    return $ Just(BS.concat [payload, fromJust remainder])
                )
                nextBlockNum)

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
-- nextDataBlockNum packet@(DATA blockNum payload) = trace("DATA packet length = " ++ show (dataLength packet) ++ ", expected = " ++ show dataSize) (if dataLength packet == dataSize then Just(blockNum+1) else Nothing)
nextDataBlockNum packet@(DATA blockNum payload) = if dataLength packet == dataSize then Just(blockNum+1) else Nothing

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
