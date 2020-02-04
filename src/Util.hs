module Util where

import Data.Word
import Data.Char
import qualified Data.ByteString as BS
import Control.Monad

-- Source: https://stackoverflow.com/a/16192050/2333689
mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = zipWith f l [0..]

stringToBytes :: String -> [Word8]
stringToBytes str = map (toEnum.ord) str

stringToByteString :: String -> BS.ByteString
stringToByteString = (BS.pack).stringToBytes

-- Run a list of monadic actions until one returns False. Source: https://stackoverflow.com/a/1134116/2333689
sequenceWhile_ :: (Monad m) => (a -> Bool) -> [m a] -> m ()
sequenceWhile_ predicate actions = foldr (\a1 a2 -> a1 >>= \x -> when (predicate x) a2) (return ()) actions