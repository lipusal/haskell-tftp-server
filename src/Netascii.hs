{-# LANGUAGE PatternSynonyms #-} -- To be able to use constant variables in pattern matching

module Netascii where

import Data.Word
import qualified Data.ByteString as BS
import Data.Char
import Debug.Trace

minValue = 0
maxValue = 0x7F

-- Idea from https://stackoverflow.com/a/46311245/2333689
pattern LF = 10
pattern CR = 13

isNetascii :: BS.ByteString -> Bool
isNetascii str = BS.all isInRange str

encodeLineEndings :: BS.ByteString -> BS.ByteString
encodeLineEndings line = trace("Line to encode: " ++ show line) (if BS.null line then line else BS.concat [processedPrefix, processedSuffix]) where
    (prefix, suffix) = BS.break isLineBreak line
    (processedPrefix, charsConsumed) = encodeChunk prefix suffix
    processedSuffix = trace("Encoded chunk: " ++ show (processedPrefix, charsConsumed)) (if BS.null suffix then suffix else encodeLineEndings safeSuffix)
    safeSuffix = BS.drop charsConsumed suffix
    a = trace("Prefix: " ++ show prefix ++ "(" ++ show(BS.length prefix) ++ "), processed prefix: " ++ show processedPrefix ++ " (" ++ show(BS.length processedPrefix) ++ "), suffix: " ++ show suffix ++ " (" ++ show(BS.length suffix) ++ "), safe processed suffix: " ++ show safeSuffix ++ " (" ++ show(BS.length safeSuffix) ++ ")") 3

--- PRIVATE

isInRange :: Word8 -> Bool
isInRange char = minValue <= char && char <= maxValue

isLineBreak :: Word8 -> Bool
isLineBreak x = x == LF || x == CR -- TODO reduce

encodeChunk :: BS.ByteString -> BS.ByteString -> (BS.ByteString, Int)
encodeChunk prefix suffix = trace("Chunk to encode: " ++ show prefix ++ ", " ++ show suffix ++ ", null suffix? " ++ show(BS.null suffix)) (if BS.null suffix then (prefix, 0) else (BS.concat[prefix, processedSuffix], charsConsumed)) where
    char1 = BS.head suffix
    char2 = if BS.length suffix < 2 then Nothing else Just(BS.head (BS.tail suffix))
    (rawProcessedSuffix, charsConsumed) = encodePair char1 char2
    processedSuffix = trace("char1: " ++ show char1 ++ ", char2: " ++ show char2 ++ ", processed: " ++ show rawProcessedSuffix ++ ", chars consumed: " ++ show charsConsumed) BS.pack rawProcessedSuffix

-- Insert CR/LF as necessary and inform how many characters of the original string were consumed
encodePair :: Word8 -> Maybe Word8 -> ([Word8], Int)
encodePair CR Nothing = ([CR, LF], 1)
encodePair LF Nothing = ([CR, LF], 1)
encodePair x Nothing = ([x], 1)
encodePair CR (Just LF) = ([CR, LF], 2)
encodePair CR (Just x) = ([CR, LF, x], 2)
encodePair LF (Just x) = ([CR, LF, x], 2)
encodePair x (Just y) = ([x, y], 2)


-- TODO delete from here onwards and delete import Data.Char
stringToBytes :: String -> [Word8]
stringToBytes str = map (toEnum.ord) str

-- import qualified Data.ByteString as BS
-- a = Data.ByteString.pack [10, 72, 101, 108, 108, 111, 10, 119, 111, 114, 108, 100, 13]
-- a = Data.ByteString.pack(stringToBytes "netascii:\n- Modified form of ASCII, defined in RFC 764: 8-bit extension of the 7-bit ASCII character space\n- From 0x20 to 0x7F (the printable characters and the space) and eight of the control characters")
-- encodeLineEndings a

