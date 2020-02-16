{-# LANGUAGE PatternSynonyms #-} -- To be able to use constant variables in pattern matching

module Netascii (netasciiEncode, netasciiDecode, isNetascii, minValue, maxValue) where

import Data.Word
import qualified Data.ByteString as BS
import qualified System.IO
import Util

minValue = 0x20
maxValue = 0x7F
controlChars = [0x00, LF, CR, 0x07, 0x08, 0x09, 0x11, 0x12] -- Control characters allowed in netascii, https://stackoverflow.com/a/10936875/2333689

-- Idea from https://stackoverflow.com/a/46311245/2333689
pattern LF = 10
pattern CR = 13

netasciiEncode :: BS.ByteString -> Maybe BS.ByteString
netasciiEncode str = if isNetascii str then Just(encodeLineEndings str) else Nothing

netasciiDecode :: BS.ByteString -> Maybe BS.ByteString
netasciiDecode str = if isNetascii str then Just(decodeLineEndings str) else Nothing

isNetascii :: BS.ByteString -> Bool
isNetascii str = BS.all isInRange str

--- PRIVATE

isInRange :: Word8 -> Bool
isInRange char = (minValue <= char && char <= maxValue) || elem char controlChars

isLineBreak :: Word8 -> Bool
isLineBreak x = x == LF || x == CR

isCRLF :: Word8 -> Word8 -> Bool
isCRLF CR LF = True
isCRLF _ _ = False

isCRLF' :: BS.ByteString -> Bool
isCRLF' str = BS.length str == 2 && isCRLF x y where
    x = BS.head str
    y = BS.last str

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

-- Decode CRLF to system-native line break
decodeLineEndings :: BS.ByteString -> BS.ByteString
decodeLineEndings line = BS.concat decodedChunks where
    decodedChunks = map decodeChunk chunks
    chunks = BS.groupBy isCRLF line

decodeChunk :: BS.ByteString -> BS.ByteString
decodeChunk chunk
    | isCRLF' chunk = if System.IO.nativeNewline == System.IO.CRLF then chunk else BS.singleton LF
    | otherwise = chunk
