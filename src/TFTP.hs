module TFTP where

import Data.Word
import Data.Char
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString
import Debug.Trace
import System.IO
import System.IO.Error

blockSize = 512

data Packet =
    RRQ String String -- opcode 1 + filename + 0 + mode + 0
    | WRQ String String -- opcode 2 + filename + 0 + mode + 0
    | DATA Word16 Data.ByteString.ByteString -- opcode 3 + block number + data TODO also consider strict/lazy bytestrings
    | DATA2 Word16 [Word8] -- opcode 3 + block number + data TODO also consider strict/lazy bytestrings
    | ACK Word16 -- opcode 4 + block number
    | ERROR Word16 String deriving Show -- opcode 5 + error code + error message + 0

fromByteString :: Data.ByteString.ByteString -> Maybe Packet
fromByteString bs = fromOpcode opcode payload
    where (header,payload) = Data.ByteString.splitAt 2 bs
          opcode = byteStringToWord16 header

fromOpcode :: Word16 -> Data.ByteString.ByteString -> Maybe Packet
fromOpcode 1 d = Just (RRQ filename mode)
    where (filename:mode:_) = map byteStringtoString (Data.ByteString.split 0 d)
fromOpcode 2 d = Just (WRQ filename mode)
    where (filename:mode:_) = map byteStringtoString (Data.ByteString.split 0 d)
fromOpcode 3 d = Just (DATA blockNum payload)
    where (header,payload) = Data.ByteString.splitAt 2 d
          blockNum = byteStringToWord16 header
fromOpcode 4 d = Just (ACK (byteStringToWord16 d))
fromOpcode 5 d = Just (ERROR errCode errMsg)
    where (header,errBytes) = Data.ByteString.splitAt 2 d
          errCode = byteStringToWord16 header
          errMsg = byteStringtoString (Data.ByteString.init errBytes) -- init to remove the trailing 0
fromOpcode x y = trace ("Received opcode=" ++ show x ++ " and data=" ++ byteStringtoString y) Nothing


process :: Packet -> IO ([Packet])
process (RRQ filename mode) = fileToPackets filename (map toLower mode)
process (WRQ filename mode) = return [ACK 0] -- TODO: We have to save state to know what we're reading from. Use TIDs
process (DATA blockNum payload) = return [ACK blockNum] -- TODO: Write to file, we have to have a session set to know file name
process (ACK blockNum) = return [] -- TODO: Send more data on RRQ, or close connection, etc.
process (ERROR errNum errMsg) = trace ("ERROR " ++ show errNum ++ ": " ++ errMsg) return []

fileToPackets :: String -> String -> IO ([Packet])
fileToPackets filename "netascii" = (do
    -- TODO the only thing that changes here is openFile/openBinaryFile. DRY
    handle <- openFile filename ReadMode
    contents <- Data.ByteString.hGetContents handle
    return (mapWithIndex (\dat index -> DATA (fromIntegral (index+1)) dat) (chunks contents))
    ) `catchIOError` fileReadHandler
fileToPackets filename "octet" = (do
    handle <- openBinaryFile filename ReadMode
    contents <- Data.ByteString.hGetContents handle
    return (mapWithIndex (\dat index -> DATA (fromIntegral (index+1)) dat) (chunks contents))
    ) `catchIOError` fileReadHandler
fileToPackets filename mode = return [ERROR 0 ("Invalid mode " ++ mode)]

fileReadHandler :: IOError -> IO ([Packet])
fileReadHandler e
    | isAlreadyInUseError e = return [ERROR 2 "File already in use"]
    | isDoesNotExistError e = return [ERROR 1 "File not found"]
    | isPermissionError e = return [ERROR 2 ("Permission error: " ++ ioeGetErrorString e)]
    | otherwise = return [ERROR 2 ("Error: " ++ ioeGetErrorString e)]

-- Split a bytestring into chunks of size blockSize
chunks :: Data.ByteString.ByteString -> [Data.ByteString.ByteString]
chunks b = if Data.ByteString.null b then [] else chunk:(chunks remainder)
    where (chunk, remainder) = Data.ByteString.splitAt blockSize b

-- Source: https://stackoverflow.com/a/16192050/2333689
mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = zipWith f l [0..]


fromByteString2 :: Data.ByteString.ByteString -> Maybe Packet
fromByteString2 d = fromCharArray2 opcode16 rest
    where (opHigh:opLow:rest) = Data.ByteString.unpack d
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


byteStringtoString :: Data.ByteString.ByteString -> String
byteStringtoString = map (chr.fromEnum).Data.ByteString.unpack

singleWord8to16 :: Word8 -> Word16
singleWord8to16 = fromInteger.toInteger

-- Convert the first two bytes of the given bytestring to a 2-byte integer
byteStringToWord16 :: Data.ByteString.ByteString -> Word16
-- return ((higher << 8) | lower) :: Word16
byteStringToWord16 bs = fromIntegral ((.|.) (high `shiftL` 8) (low))
    where (high:low:_) = map fromEnum (Data.ByteString.unpack bs)
        --   (high2:low2:_) = (Data.ByteString.unpack bs)
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


