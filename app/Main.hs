module Main where

import qualified TFTP
import qualified UDP

main :: IO ()
main = do
    UDP.runUDPServer
