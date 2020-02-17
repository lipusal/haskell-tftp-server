module UDP where

import Control.Concurrent        (forkIO, threadDelay)
import Control.Monad             (forever)
import Network.Socket hiding     (recv, recvFrom)
import Network.Socket.ByteString (recv, recvFrom)
import qualified Data.ByteString (ByteString)
import qualified TFTP

-- SOURCE: http://www.mchaver.com/posts/2017-06-12-haskell-network-programming-1.html

runUDPServer :: Integer -> IO ()
runUDPServer portNum = do
  masterSocket <- openPort $ Just $ show portNum
  actualPortNum <- socketPort masterSocket
  putStrLn $ "Started server on port " ++ show actualPortNum
  forever $ mainLoop masterSocket

mainLoop :: Socket -> IO ()
mainLoop listenSock = do
  (rxData, txSocketAddr) <- recvFrom listenSock 1024
  socket <- openPort Nothing  -- Open next available port
  connect socket txSocketAddr -- Connect to client on other end
  portNum <- socketPort socket
  putStrLn("Established socket pair <127.0.0.1:" ++ show portNum ++ ", " ++ show txSocketAddr ++ "> for new connection")
  forkIO $ handle portNum txSocketAddr rxData socket
  -- handle portNum txSocketAddr rxData socket
  return ()

resolve :: String -> Maybe String -> IO (AddrInfo)
resolve addr portNum = do
  -- Resolve addr:portNum. Returns array of possible values, best is first
  addrinfos <- getAddrInfo Nothing (Just addr) portNum
  return(head addrinfos)

openPort :: Maybe String -> IO (Socket)
openPort portNum = do
  serverAddr <- resolve "0.0.0.0" portNum
  result <- socket AF_INET Datagram defaultProtocol
  -- Bind socket (ie. start listening)
  bind result (addrAddress serverAddr)
  return result

handle :: PortNumber -> SockAddr -> Data.ByteString.ByteString -> Socket -> IO ()
handle portNum txSocketAddr rxData socket = do
  TFTP.handle rxData socket
  putStrLn("Connection <127.0.0.1:" ++ show portNum ++ ", " ++ show txSocketAddr ++ "> complete, closing socket")
  close' socket -- close' to throw exception if underlying system throws exception

connectTo :: String -> String -> IO(Socket)
connectTo addr port = do
  socket <- openPort Nothing -- receiving side
  addrInfo <- resolve addr (Just port)
  connect socket (addrAddress addrInfo) -- transmitting side
  return socket