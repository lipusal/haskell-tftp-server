{-# LANGUAGE PatternSynonyms #-} -- To be able to use constant variables in pattern matching

module Netascii (netasciiEncode, netasciiDecode, isNetascii) where

import Data.Word
import qualified Data.ByteString as BS

minValue = 0
maxValue = 0x7F

-- Idea from https://stackoverflow.com/a/46311245/2333689
pattern LF = 10
pattern CR = 13

netasciiEncode :: BS.ByteString -> Maybe BS.ByteString
netasciiEncode str = if isNetascii str then Just(encodeLineEndings str) else Nothing

-- TODO NOW decode
netasciiDecode :: BS.ByteString -> Maybe BS.ByteString
netasciiDecode = netasciiEncode

isNetascii :: BS.ByteString -> Bool
isNetascii str = BS.all isInRange str

--- PRIVATE

isInRange :: Word8 -> Bool
isInRange char = minValue <= char && char <= maxValue

isLineBreak :: Word8 -> Bool
isLineBreak x = x == LF || x == CR -- TODO reduce

encodeLineEndings :: BS.ByteString -> BS.ByteString
encodeLineEndings line = if BS.null line
    then line
    else BS.concat [processedPrefix, processedSuffix] where
        (prefix, suffix) = BS.break isLineBreak line
        (processedPrefix, charsConsumed) = encodeChunk prefix suffix
        processedSuffix = if BS.null suffix
            then suffix
            else encodeLineEndings safeSuffix
        safeSuffix = BS.drop charsConsumed suffix

-- Encode a chunk boundary, also returning how many characters from the suffix were consumed
encodeChunk :: BS.ByteString -> BS.ByteString -> (BS.ByteString, Int)
encodeChunk prefix suffix = if BS.null suffix
    then (prefix, 0)
    else (BS.concat[prefix, processedSuffix], charsConsumed) where
        char1 = BS.head suffix
        char2 = if BS.length suffix < 2
            then Nothing
            else Just $ BS.head $ BS.tail suffix
        (rawProcessedSuffix, charsConsumed) = encodePair char1 char2
        processedSuffix = BS.pack rawProcessedSuffix

-- Insert CR/LF as necessary and inform how many characters of the original string were consumed
encodePair :: Word8 -> Maybe Word8 -> ([Word8], Int)
encodePair CR Nothing = ([CR, LF], 1)
encodePair LF Nothing = ([CR, LF], 1)
encodePair x Nothing = ([x], 1)
encodePair CR (Just LF) = ([CR, LF], 2)
encodePair CR (Just x) = ([CR, LF, x], 2)
encodePair LF (Just x) = ([CR, LF, x], 2)
encodePair x (Just y) = ([x, y], 2)
