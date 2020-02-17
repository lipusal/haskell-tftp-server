module Main where

import qualified TFTP
import qualified TFTP.Files
import TFTP.Packet
import TFTP.Conversions
import qualified UDP
import Data.Maybe
import Data.Either
import System.Exit
import Control.Monad
import System.Environment
import Data.Word
-- import Network.Socket (Socket, close', connect)
import Network.Socket hiding (recvFrom)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (sendAllTo, recvFrom)
import System.IO
import Netascii

main :: IO ()
main = do
    eitherArgs <- parseArgs
    either
        (\_ -> die "Invalid arguments, exiting")
        doTheThing
        eitherArgs

doTheThing :: (String, String, String, String) -> IO()
doTheThing (addr, port, file, localFile) = do
    socket <- UDP.openPort Nothing
    localAddr <- getSocketName socket
    putStrLn("Opened socket " ++ show localAddr ++ " for new connection")


    putStrLn "Sending initial packet"
    let initialPacket = RRQ file "caesar"
    remoteAddr <- UDP.resolve addr $ Just port
    sendAllTo socket (serialize initialPacket) $ addrAddress remoteAddr
    putStrLn(">>>> " ++ show initialPacket)

    putStrLn "Initial packet sent, waiting for response..."
    
    -- Reply will come back from a different port
    (rxData, txSocketAddr) <- recvFrom socket 1024
    putStrLn $ "Received " ++ show rxData ++ " from " ++ show txSocketAddr
    connect socket txSocketAddr -- Connect to new port

    -- Parse response, if necessary retry until successful or ERROR packet
    let maybeKeyPacket = deserialize rxData
    putStrLn("<<<< " ++ show maybeKeyPacket)
    eitherKey <- receiveKey' socket maybeKeyPacket
    when (isLeft eitherKey) (die "Couldn't get Caesar key")
    
    let key = fromRight 42 eitherKey -- Will never default to 42
    putStrLn $ "Caesar key: " ++ show key
    TFTP.sendPacket socket (ACK 0)

    eitherHandle <- TFTP.Files.openFileHandler localFile WriteMode "caesar"
    either
        (TFTP.sendPacket socket)  -- Partial application, this receives the error packet
        (\handle -> do
            maybeFileContents <- TFTP.recvFile "caesar" socket 1
            -- when (isJust maybeFileContents) $ BS.hPut handle $ fromJust $ netasciiDecode $ fromJust maybeFileContents
            when (isJust maybeFileContents) $ BS.hPut handle $ prettyFile key $ fromJust maybeFileContents
            hClose handle
        )
        eitherHandle

    putStrLn "Transaction complete, closing socket"
    close' socket
    return ()

prettyFile :: Word16 -> BS.ByteString -> BS.ByteString
prettyFile key contents = BS.concat[stringToByteString $ "Caesar key: " ++ show key ++ "\n", contents]

-- Attempt to receive key. If receiving ERROR packet, return the packet, otherwise retry until key is obtained.
receiveKey :: Socket -> IO (Either Packet Word16)
receiveKey socket = do
    maybePacket <- TFTP.recvPacket socket
    receiveKey' socket maybePacket

receiveKey' :: Socket -> Maybe Packet -> IO (Either Packet Word16)
receiveKey' socket Nothing = receiveKey socket -- Invalid data, retry
receiveKey' socket (Just packet@(ERROR _ _)) = return $ Left packet -- Received ERROR, abort
receiveKey' socket (Just (DATA 0 payload)) = return $ Right $ byteStringToWord16 payload -- Extract key
receiveKey' socket _ = receiveKey socket -- Not a DATA 0 packet, retry


parseArgs :: IO (Either () (String, String, String, String))
parseArgs = do
    args <- getArgs
    return (if length args == 0 then Left () else Right $ parseArgs' args)

parseArgs' :: [String] -> (String, String, String, String)
parseArgs' [addr,file] = (addr, "69", file, file)
parseArgs' [addr,port,file] = (addr, port, file, file)
parseArgs' [addr,port,file,localFile] = (addr, port, file, localFile)
