module TFTP.Files (fileToPackets, openFileHandler) where

import TFTP.Packet
import TFTP.Constants
import qualified Data.ByteString as BS
import System.IO
import System.IO.Error
import Netascii
import Util
import Data.Maybe
import Data.Either

-- Attempt to read a file and split it into packets. If there's an error opening the file, return appropriate error packet.
fileToPackets :: String -> String -> IO (Either Packet [Packet])
fileToPackets filename mode = do
    eitherHandle <- openFileHandler filename ReadMode mode
    either
        (\errorPacket -> return $ Left errorPacket)
        (\handle -> handleToPackets handle mode)
        eitherHandle

openFileHandler :: FilePath -> IOMode -> String -> IO (Either Packet Handle)
openFileHandler path mode "netascii" = (do
    handle <- openFile path mode
    return(Right handle)) `catchIOError` (\e -> return $ Left $ ioErrorHandler e)
openFileHandler path mode "octet" = (do
        handle <- openBinaryFile path mode
        return(Right handle)) `catchIOError` (\e -> return $ Left $ ioErrorHandler e)
openFileHandler _ _ tftpMode = return $ Left(ERROR 4 ("Invalid mode " ++ tftpMode))

-- PRIVATE

handleToPackets :: Handle -> String -> IO (Either Packet [Packet])
handleToPackets handle "netascii" = do
    contents <- BS.hGetContents handle
    hClose handle
    let encodedContents = netasciiEncode contents
    let packerFn = \encodedContents -> Right $ fileContentsToPackets encodedContents
    let errPacket = Left(ERROR 0 "File contents are outside netascii range, use octet mode")
    let result = maybe errPacket packerFn encodedContents
    return result
handleToPackets handle "octet" = do
    contents <- BS.hGetContents handle
    hClose handle
    let result = Right $ fileContentsToPackets contents
    return result

fileContentsToPackets :: BS.ByteString -> [Packet]
fileContentsToPackets contents = mapWithIndex (\dat index -> DATA (fromIntegral(index+1)) dat) (chunks contents)

ioErrorHandler :: IOError -> Packet
ioErrorHandler e
    | isAlreadyInUseError e = ERROR 2 "File already in use"
    | isDoesNotExistError e = ERROR 1 "File not found"
    | isPermissionError e = ERROR 2 ("Permission error: " ++ ioeGetErrorString e)
    | otherwise = ERROR 2 ("Error: " ++ ioeGetErrorString e)

-- Split a bytestring into chunks of size dataSize
chunks :: BS.ByteString -> [BS.ByteString]
chunks b = if BS.null b then [] else chunk:(chunks remainder)
    where (chunk, remainder) = BS.splitAt dataSize b
