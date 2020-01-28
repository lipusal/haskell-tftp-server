{-# LANGUAGE PatternSynonyms #-}

module Netascii where

import Data.Word
import qualified Data.ByteString as BS

minValue = 0
maxValue = 0x7F

-- Idea from https://stackoverflow.com/a/46311245/2333689
pattern LF = 10
pattern CR = 13

isInRange :: Word8 -> Bool
isInRange char = minValue <= char && char <= maxValue

-- encodeLineEndings :: BS.ByteString -> BS.ByteString
-- encodeLineEndings str = 

isLineBreak :: Word8 -> Bool
isLineBreak x = x == LF || x == CR -- TODO reduce

encodeLineEndings :: BS.ByteString -> BS.ByteString
encodeLineEndings line = BS.concat [processedPrefix, processedSuffix] where
    (prefix, suffix) = BS.break isLineBreak line
    processedPrefix = encode prefix suffix
    processedSuffix = if BS.null suffix then suffix else encodeLineEndings suffix

encode :: BS.ByteString -> BS.ByteString -> BS.ByteString
encode prefix suffix = BS.concat[prefix, processedSuffix] where
    char1 = BS.head suffix
    char2 = if BS.length suffix < 2 then Nothing else Just(BS.head (BS.tail suffix))
    processedSuffix = BS.pack(thingy char1 char2)

thingy :: Word8 -> Maybe Word8 -> [Word8] -- TODO rename
thingy CR Nothing = [CR, LF]
thingy LF Nothing = [CR, LF]
thingy x Nothing = [x]
thingy CR (Just LF) = [CR, LF]
thingy CR (Just x) = [CR, LF, x]
thingy LF (Just x) = [CR, LF, x]
thingy x (Just y) = [x, y]

-- a = pack [10, 72, 101, 108, 108, 111, 10, 119, 111, 114, 108, 100, 13]

