module Main where

import Lib
import qualified TFTP
import qualified UDP

main :: IO ()
main = do
    helloWorld
    print (TFTP.RRQ "/a/b/c" "octet")
    UDP.runUDPServer
