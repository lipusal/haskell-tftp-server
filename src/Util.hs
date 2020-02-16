module Util where

import Data.Word
import Data.Char
import qualified Data.ByteString as BS
import Control.Monad
import System.Random

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

-- Helps convert between Int, Integer, Word*, etc.
numberConversion :: Integral a => Num b => a -> b
numberConversion = fromIntegral.toInteger

-- Generate random number, inspired from http://zvon.org/other/haskell/Outputrandom/randomR_f.html and official doc https://hackage.haskell.org/package/random-1.1/docs/System-Random.html#t:RandomGen
randomNum :: Int -> Int -> IO(Int)
randomNum x y = getStdRandom $ randomR (x,y)

randomNum' :: Word8 -> Word8 -> IO(Word16)
randomNum' x y = do
    rawResult <- randomNum (numberConversion x) (numberConversion y) 
    return $ numberConversion rawResult