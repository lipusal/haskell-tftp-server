module Main where

import Lib
import qualified TFTP
import qualified UDP

main :: IO ()
main = do
    helloWorld
    UDP.runUDPServer
