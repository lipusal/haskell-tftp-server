module TFTP
    (
        startServer,
        doTheThing
    ) where

import qualified Network.TFTP.Server as Server
import qualified System.FilePath as FilePath

doTheThing :: String
doTheThing = "The thing!"
-- doTheThing = do
--  result <- startServer
--  return handleMaybe result

-- handleMaybe :: Maybe String -> String
-- handleMaybe Nothing = "OK"
-- handleMaybe Just x = x

startServer :: IO (Maybe String)
startServer = Server.singleBinary (Just 5000) ("/Users/jlipuma/tftp-server/README.md") "alias" Nothing (Just "9000")
-- startServer = Server.singleBinary (Just 5000) ("/Users/jlipuma/tftp-server/README.md") "alias" (Just "localhost") (Just "9000")
