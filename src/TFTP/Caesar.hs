module TFTP.Caesar (caesarEncode) where

import TFTP.Packet
import qualified Netascii (minValue, maxValue)
import Util
import qualified Data.ByteString as BS
import Data.Word

-- CAESAR MODE - only for RRQ
-- Caesar encoding is when all characters are offset by the same number. The "key" is the number.
-- 1) Client => Sever: RRQ file "caesar"
-- 2) Server => Client: DATA 0 key
-- 3) Client => Server: ACK 0
-- 4) Normal RRQ transfer. Transferred content will be Caesar-encoded with the given key

-- Returns a function that, given a ByteString, shifts all netascii contents by `key` number of characters (Caesar encode)
caesarEncode :: Word16 -> (BS.ByteString -> BS.ByteString)
caesarEncode key = BS.map $ caesarEncodeWord (numberConversion key) (numberConversion Netascii.maxValue)

-- PRIVATE

caesarEncodeWord :: Int -> Int -> (Word8 -> Word8)
caesarEncodeWord key maxValue = toEnum.(caesarEncode' key maxValue).fromEnum

-- Actual Caesar encode: Shift a char as int
caesarEncode' :: Int -> Int -> Int -> Int
caesarEncode' key maxValue c = (c + key) `mod` maxValue
