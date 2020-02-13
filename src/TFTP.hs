module TFTP (handle) where

import Data.Word
import Data.Char
import qualified Data.ByteString as BS
import TFTP.Packet
import TFTP.Files
import TFTP.Constants
import Debug.Trace
import System.IO hiding (hGetContents, hPut)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import Data.Maybe
import Data.Either
import Netascii
import Control.Monad
import Util (sequenceWhile_)

handle :: BS.ByteString -> Socket -> IO ()
handle payload socket = do
    let initialPacket = deserialize payload
    putStrLn("<<<< " ++ show initialPacket)
    if isNothing initialPacket
        then sendPacket socket (ERROR 4 "Not a TFTP packet")
        else handleInitialPacket (fromJust initialPacket) socket

-- PRIVATE

handleInitialPacket :: Packet -> Socket -> IO()
handleInitialPacket packet@(RRQ filename mode) socket = handleRRQ packet socket
handleInitialPacket packet@(WRQ filename mode) socket = handleWRQ packet socket
handleInitialPacket _ socket = sendPacket socket (ERROR 4 "Not a valid initial TFTP packet")

sendPacket :: Socket -> Packet -> IO()
sendPacket sock packet = trace(">>>> " ++ show packet) sendAll sock (serialize packet)

recvPacket :: Socket -> IO(Maybe Packet)
recvPacket socket = do
    recvData <- recv socket packetSize
    let result = deserialize recvData
    trace("<<<< " ++ show result) return result

-- (Re)send data packet until appropriate ACK or ERROR is received
sendDataPacket :: Socket -> Packet -> IO (Bool)
sendDataPacket socket packet@(DATA blockNum payload) = do
    sendPacket socket packet
    response <- recvPacket socket
    let isOk = isAck blockNum response
    let isErr = isError response
    when isErr $ putStrLn("Received error packet, stopping transfer")
    if isErr || isOk
        then return isOk
        else sendDataPacket socket packet

-- Receive data until receiving a DATA packet with the expected block number
recvDataPacket :: Socket -> Word16 -> IO(Packet, Maybe Word16)
recvDataPacket sock expectedBlockNum = do
    packet <- recvPacket sock
    if (isData expectedBlockNum packet)
        then return (fromJust packet, nextDataBlockNum $ fromJust packet)
        else recvDataPacket sock expectedBlockNum -- Retry until successful

handleRRQ :: Packet -> Socket -> IO()
handleRRQ (RRQ filename mode) socket = do
    eitherPackets <- fileToPackets filename (map toLower mode)
    either
        (\errPacket -> sendPacket socket errPacket)
        -- (\filePackets -> sendFile filePackets socket)
        (\filePackets -> sendFile' filePackets socket)
        eitherPackets

sendFile :: [Packet] -> Socket -> IO ()
sendFile filePackets socket = mapM_ (sendDataPacket socket) filePackets

-- Stops at first ERROR packet received
sendFile' :: [Packet] -> Socket -> IO ()
sendFile' filePackets socket = sequenceWhile_ (== True) $ map (sendDataPacket socket) filePackets

handleWRQ :: Packet -> Socket -> IO ()
handleWRQ (WRQ filename mode) socket = do
    eitherHandle <- openFileHandler filename WriteMode mode
    either
        (sendPacket socket)  -- Partial application, this receives the error packet
        (\handle -> do
            sendPacket socket (ACK 0)
            maybeFileContents <- recvFile mode socket 1
            let maybeDecodedFileContents = if mode == "octet" then maybeFileContents else (maybe Nothing netasciiDecode maybeFileContents)
            when (isJust maybeDecodedFileContents) $ BS.hPut handle $ fromJust maybeDecodedFileContents
            hClose handle
        )
        eitherHandle

recvFile :: String -> Socket -> Word16 -> IO(Maybe BS.ByteString)
recvFile mode sock blockNum = do
    (packet, nextBlockNum) <- recvDataPacket sock blockNum
    let payload = dataPayload packet
    if mode == "netascii" && (not $ isNetascii payload)
        then (do
            sendPacket sock (ERROR 0 "File contents are outside netascii range, use octet mode")
            return Nothing
        ) else (do 
            sendPacket sock (ACK blockNum)
            maybe
                (return $ Just payload) -- Transfer complete
                (\nextBlock -> do
                    remainder <- recvFile mode sock nextBlock
                    return $ Just $ BS.concat [payload, fromJust remainder]
                )
                nextBlockNum
        )
