module Main where

import Lib
import TFTP
import qualified Network.TFTP.Server as Server

main = Server.singleBinary (Just 5000) "/Users/jlipuma/tftp-server/README.md" "a" Nothing (Just "9000")

-- main :: IO ()
-- main = do
--     putStrLn (Just "About to start server")
--     serverResult <- startServer
-- --     serverResult <- doTheThing
--     putStrLn (Just "Server started")
--     putStrLn serverResult
-- --     print serverResult
