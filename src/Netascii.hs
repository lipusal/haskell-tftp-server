module Netascii where

import Data.Word
import qualified Data.ByteString as BS

minValue = 0
maxValue = 0x7F
lf = 10
cr = 13

isInRange :: Word8 -> Bool
isInRange char = minValue <= char && char <= maxValue

-- encodeLineEndings :: BS.ByteString -> BS.ByteString
-- encodeLineEndings str = 


encode :: Word8 -> Word8 -> [Word8]
encode cr lf = [cr, lf]
encode cr x = [cr, lf, x]
encode lf x = [cr, lf, x]
encode x y = [x, y]
