module TFTP.Packet where

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Builder
import TFTP.Constants
import TFTP.Conversions

data Packet =
    RRQ String String -- opcode 1 + filename + 0 + mode + 0
    | WRQ String String -- opcode 2 + filename + 0 + mode + 0
    | DATA Word16 BS.ByteString -- opcode 3 + block number + data TODO also consider lazy bytestrings
    | ACK Word16 -- opcode 4 + block number
    | ERROR Word16 String -- opcode 5 + error code + error message + 0
    deriving Show

-- Check if the given package is a DATA package with the specified block number
isData :: Word16 -> Maybe Packet -> Bool
isData expectedBlockNum (Just(DATA blockNum _)) = blockNum == expectedBlockNum
isData _ _ = False

dataPayload :: Packet -> BS.ByteString
dataPayload (DATA _ payload) = payload

dataLength :: Packet -> Int
dataLength = (BS.length).dataPayload

-- If the packet is full-length, get the next block number. Otherwise return Nothing.
nextDataBlockNum :: Packet -> Maybe Word16
nextDataBlockNum packet@(DATA blockNum _) = if dataLength packet == dataSize
    then Just(blockNum+1)
    else Nothing
 
-- Checks if given package is an ACK for the specified block number
isAck :: Word16 -> Maybe Packet -> Bool
isAck expectedBlockNum (Just(ACK blocknum)) = blocknum == expectedBlockNum
isAck _ _ = False

deserialize :: BS.ByteString -> Maybe Packet
deserialize bs = fromOpcode opcode payload
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
fromOpcode 4 d = Just $ ACK (byteStringToWord16 d)
fromOpcode 5 d = Just (ERROR errCode errMsg)
    where (header,errBytes) = BS.splitAt 2 d
          errCode = byteStringToWord16 header
          errMsg = byteStringtoString $ BS.init errBytes -- init to remove the trailing 0
fromOpcode _ _ = Nothing

-- Create the builder, execute it (can only generate lazy bytestrings), and convert to strict bytestring
serialize :: Packet -> BS.ByteString
serialize packet = (BSL.toStrict).toLazyByteString $ serializer packet

serializer :: Packet -> Builder
serializer (RRQ filename mode) = word16LE 1
    <> stringUtf8 filename
    <> word8 0
    <> string7 mode
    <> word8 0
serializer (WRQ filename mode) = word16LE 2
    <> stringUtf8 filename
    <> word8 0
    <> string7 mode
    <> word8 0
serializer (DATA blocknum payload) = word16LE 3
    <> word16LE blocknum
    <> byteString payload
serializer (ACK blocknum) = word16LE 4
    <> word16LE blocknum
serializer (ERROR errNum errMsg) = word16LE 5
    <> word16LE errNum
    <> stringUtf8 errMsg
    <> word8 0