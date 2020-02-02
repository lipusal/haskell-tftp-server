module TFTP.Conversions where

import qualified Data.ByteString as BS
import Data.Word
import Data.Char
import Data.Bits (shiftL, shiftR, (.&.), (.|.))

byteStringtoString :: BS.ByteString -> String
byteStringtoString = map (chr.fromEnum).(BS.unpack)

stringToByteString :: String -> BS.ByteString
stringToByteString str = BS.pack $ map (toEnum.ord) str

-- Convert the first two bytes of the given bytestring to a 2-byte integer
-- Basically, ((higher << 8) | (lower & 255)) :: Word16
byteStringToWord16 :: BS.ByteString -> Word16
byteStringToWord16 bs = fromIntegral $ (.|.) (high `shiftL` 8) ((.&.) low 255)
    where (high:low:_) = map fromEnum $ BS.unpack (BS.take 2 bs)

-- Basically BS.pack [num >> 8, num & 255]
word16ToByteString :: Word16 -> BS.ByteString
word16ToByteString num = BS.pack [higher,lower]
    where higher = fromInteger $ (toInteger num) `shiftR` 8
          lower = fromInteger (toInteger $ (.&.) num 255)
