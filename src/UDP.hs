module UDP where

import Control.Concurrent        (forkIO, threadDelay)
import Control.Monad             (forever)
-- import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv, recvFrom, send, sendAll, sendAllTo)
import Network.Socket.ByteString (recv, recvFrom, send, sendAll, sendAllTo)
import Data.Maybe
import qualified Data.ByteString (concat,length)
import TFTP

-- SOURCE: http://www.mchaver.com/posts/2017-06-12-haskell-network-programming-1.html

runUDPServer :: IO ()
runUDPServer = do
  -- Resolve 127.0.0.1:7000. Returns array of possible values, best is first
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  let serverAddr = head addrinfos
  masterSocket <- Network.Socket.socket (addrFamily serverAddr) Datagram defaultProtocol
  -- Bind socket (ie. start listening)
  bind masterSocket (addrAddress serverAddr)
  portNum <- socketPort masterSocket
  putStrLn ("Started server on port " ++ (show portNum))
  listen masterSocket 2 -- Listen for connections. Handle up to 2 pending connections
  -- forever do (mainLoop masterSocket)
  mainLoop masterSocket

mainLoop :: Socket -> IO ()
mainLoop listenSock = do
  (recvSocket, recvSocketAddr) <- accept listenSock -- Wait until connection established

  socketData <- recv recvSocket 4096
  let tftpPacket = fromByteString socketData
  putStrLn("<<<< New request: " ++ show tftpPacket)
  handleSession tftpPacket
  mainLoop listenSock -- Handle next connection

handleSession :: Maybe Packet -> Socket -> IO ()
handleSession Nothing _ = return ()
handleSession packet socket = do
  let session = newSession socket (fromJust packet)
  handle session -- TODO NOW in TFTP

  responsePackets <- process (fromJust tftpPacket)
  putStrLn(">>>> Packets (" ++ show (length responsePackets) ++ "): " ++ showList responsePackets "")
  let response = Data.ByteString.concat (map toByteString responsePackets)
  -- putStrLn(">>>> Response data: " ++ byteStringtoString response)
  replySocket <- socket (addrFamily serverAddr) Datagram defaultProtocol
  connect replySocket recvAddr
  sendAllTo replySocket response recvAddr
