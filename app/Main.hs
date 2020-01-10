module Main where

import Lib
import qualified TFTP

main :: IO ()
main = do
    helloWorld
    print (TFTP.RRQ "/a/b/c" "octet")
