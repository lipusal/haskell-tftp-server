module Util where

import Data.Word
import Data.Char
import qualified Data.ByteString as BS

-- Source: https://stackoverflow.com/a/16192050/2333689
mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = zipWith f l [0..]

stringToBytes :: String -> [Word8]
stringToBytes str = map (toEnum.ord) str

stringToByteString :: String -> BS.ByteString
stringToByteString = (BS.pack).stringToBytes
