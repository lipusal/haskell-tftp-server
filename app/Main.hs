module Main where

import qualified UDP
import TFTP.Constants
import System.Environment
import Text.Read
import Debug.Trace

main :: IO ()
main = do
    portNum <- getPortNum
    UDP.runUDPServer portNum

getPortNum :: IO (Integer)
getPortNum = do
    args <- getArgs
    return $ parsePortNum args

parsePortNum :: [String] -> Integer
parsePortNum [] = defaultPort
parsePortNum (num:_) = maybe
    (trace ("Invalid port number, defaulting to " ++ show defaultPort) defaultPort)
    id
    (readMaybe num)
