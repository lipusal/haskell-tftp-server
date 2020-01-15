module UDP where

import Control.Concurrent        (forkIO, threadDelay)
import Control.Monad             (forever)
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)

-- SOURCE: http://www.mchaver.com/posts/2017-06-12-haskell-network-programming-1.html

runUDPServer :: IO ()
runUDPServer = do
  -- Resolve 127.0.0.1:7000. Returns array of possible values, best is first
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  -- Bind socket (ie. start listening)
  bind sock (addrAddress serveraddr)
  portNum <- socketPort sock
  putStrLn ("Started server on port " ++ (show portNum))
  forever (do
    socketData <- recv sock 4096
    putStrLn("<<<< Received data: " ++ show socketData)
    sendAll sock socketData
    putStrLn(">>>> Replied data: " ++ show socketData))

