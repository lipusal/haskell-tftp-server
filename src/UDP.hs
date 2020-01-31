module UDP where

import Control.Concurrent        (forkIO, threadDelay)
import Control.Monad             (forever)
-- import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv, recvFrom, send, sendAll, sendAllTo)
import Network.Socket.ByteString (recv, recvFrom, send, sendAll, sendAllTo)
import Data.Maybe
import qualified Data.ByteString (concat,length,pack)
import TFTP
import TFTP.Packet -- TODO export this directly from TFTP module

-- SOURCE: http://www.mchaver.com/posts/2017-06-12-haskell-network-programming-1.html

resolve :: Maybe String -> IO (AddrInfo)
resolve portNum = do
  -- Resolve 0.0.0.0:portNum. Returns array of possible values, best is first
  addrinfos <- getAddrInfo Nothing (Just "0.0.0.0") portNum
  return(head addrinfos)

openPort :: Maybe String -> IO (Socket)
openPort portNum = do
  serverAddr <- resolve portNum
  result <- socket AF_INET Datagram defaultProtocol
  -- Bind socket (ie. start listening)
  bind result (addrAddress serverAddr)
  return result

runUDPServer :: IO ()
runUDPServer = do
  masterSocket <- openPort(Just "7000")
  portNum <- socketPort masterSocket
  putStrLn ("Started server on port " ++ (show portNum))
  forever (mainLoop masterSocket)

mainLoop :: Socket -> IO ()
mainLoop listenSock = do
  (rxData, txSocketAddr) <- recvFrom listenSock 1024
  socket <- openPort Nothing  -- Open next available port
  connect socket txSocketAddr -- Connect to client on other end
  portNum <- socketPort socket
  putStrLn("Established socket pair <127.0.0.1:" ++ show portNum ++ ", " ++ show txSocketAddr ++ "> for new session")

  let tftpPacket = fromByteString rxData
  putStrLn("<<<< " ++ show tftpPacket)
  handleSession tftpPacket socket
  putStrLn("Connection <127.0.0.1:" ++ show portNum ++ ", " ++ show txSocketAddr ++ "> complete, closing socket")
  close' socket -- close' to throw exception if underlying system throws exception

handleSession :: Maybe Packet -> Socket -> IO ()
handleSession Nothing _ = return ()
handleSession packet sock = do
  let session = newSession sock (fromJust packet)
  handle session
