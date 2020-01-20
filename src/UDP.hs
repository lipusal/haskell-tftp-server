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
  let serveraddr = head addrinfos
  listenSocket <- socket (addrFamily serveraddr) Datagram defaultProtocol
  -- Bind socket (ie. start listening)
  bind listenSocket (addrAddress serveraddr)
  portNum <- socketPort listenSocket
  putStrLn ("Started server on port " ++ (show portNum))
  forever (do
    (socketData, recvAddr) <- recvFrom listenSocket 4096
    -- putStrLn("<<<< Received data: " ++ show socketData)
    let tftpPacket = fromByteString socketData
    -- let tftpPacket = fromByteString2 socketData
    putStrLn("<<<< Packet: " ++ show tftpPacket)
    if (isNothing tftpPacket) then return () else (do
      responsePackets <- process (fromJust tftpPacket)
      putStrLn(">>>> Packets (" ++ show (length responsePackets) ++ "): " ++ showList responsePackets "")
      let response = Data.ByteString.concat (map toByteString responsePackets)
      -- putStrLn(">>>> Response data: " ++ byteStringtoString response)
      replySocket <- socket (addrFamily serveraddr) Datagram defaultProtocol
      sendAllTo replySocket response recvAddr))

