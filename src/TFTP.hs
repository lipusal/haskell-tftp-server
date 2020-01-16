module TFTP where

import Data.Word
import Data.Bits (shiftL, (.&.))
import qualified Data.ByteString

data Packet =
    RRQ String String -- opcode 1 + filename + 0 + mode + 0
    | WRQ String String -- opcode 2 + filename + 0 + mode + 0
    | DATA Word16 [Word8] -- opcode 3 + block number + data TODO also consider strict/lazy bytestrings
    | ACK Word16 -- opcode 4 + block number
    | ERROR Word16 String deriving Show -- opcode 5 + error code + error message + 0

-- fromData :: Data.ByteString -> Maybe Packet
fromByteString d = fromCharArray opcode16 rest
    where (opHigh:opLow:rest) = Data.ByteString.unpack d
          opcode16 = word8to16 [opHigh,opLow]


fromCharArray :: Word16 -> [Word8] -> Maybe Packet
fromCharArray 1 d = Just (RRQ (showList d "") "")
fromCharArray 2 d = Just (WRQ (showList d "") "")
-- fromCharArray 3 (b1:b2:b) = Just (DATA (word8to16 [b1,b2]) b)
fromCharArray 1 d = Just (RRQ (showList d "") "")


singleWord8to16 :: Word8 -> Word16
singleWord8to16 n = fromInteger(toInteger n)

word8to16 :: [Word8] -> Word16
word8to16 (higher:lower:_) = (fromInteger(toInteger lower) `shiftL` 8) (.&.) fromInteger(toInteger lower)
