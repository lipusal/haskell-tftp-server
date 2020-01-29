{-# LANGUAGE PatternSynonyms #-} -- To be able to use constant variables in pattern matching

module Netascii where

import Data.Word
import qualified Data.ByteString as BS
import Debug.Trace

minValue = 0
maxValue = 0x7F

-- Idea from https://stackoverflow.com/a/46311245/2333689
pattern LF = 10
pattern CR = 13

isNetascii :: BS.ByteString -> Bool
isNetascii str = BS.all isInRange str

encodeLineEndings :: BS.ByteString -> BS.ByteString
encodeLineEndings line = trace("Line to encode: " ++ show line) BS.concat [processedPrefix, processedSuffix] where
    (prefix, suffix) = BS.break isLineBreak line
    processedPrefix = encodeChunk prefix suffix
    processedSuffix = trace("Prefix: " ++ show prefix ++ "(" ++ show(BS.length prefix) ++ "), processed prefix: " ++ show processedPrefix ++ " (" ++ show(BS.length processedPrefix) ++ "), suffix: " ++ show suffix ++ " (" ++ show(BS.length suffix) ++ ")") (if BS.null suffix then suffix else encodeLineEndings safeSuffix)
    safeSuffix = (if BS.null prefix then BS.tail suffix else suffix) -- TODO NOW we need info as to how many chars to advance
    -- TODO now when splitting, consider consuming the split chars. But we also need to know what the char was.

--- PRIVATE

isInRange :: Word8 -> Bool
isInRange char = minValue <= char && char <= maxValue

isLineBreak :: Word8 -> Bool
isLineBreak x = x == LF || x == CR -- TODO reduce

encodeChunk :: BS.ByteString -> BS.ByteString -> BS.ByteString
encodeChunk prefix suffix = BS.concat[prefix, processedSuffix] where
    char1 = BS.head suffix
    char2 = if BS.length suffix < 2 then Nothing else Just(BS.head (BS.tail suffix))
    processedSuffix = trace("char1: " ++ show char1 ++ ", char2: " ++ show char2) BS.pack(encodePair char1 char2)

encodePair :: Word8 -> Maybe Word8 -> [Word8]
encodePair CR Nothing = [CR, LF]
encodePair LF Nothing = [CR, LF]
encodePair x Nothing = [x]
encodePair CR (Just LF) = [CR, LF]
encodePair CR (Just x) = [CR, LF, x]
encodePair LF (Just x) = [CR, LF, x]
encodePair x (Just y) = [x, y]

-- import qualified Data.ByteString as BS
-- a = Data.ByteString.pack [10, 72, 101, 108, 108, 111, 10, 119, 111, 114, 108, 100, 13]
-- encodeLineEndings a

