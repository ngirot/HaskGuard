module Main (main) where

import Control.Concurrent (forkIO)
import qualified Data.ByteString as S
import Lib (address, negotiate, port, request)
import Network
import Network.Socket.ByteString (recv, sendAll)
import Streaming (stream)

main :: IO ()
main = runTCPServer Nothing "4242" talk
  where
    talk s = do
      putStrLn ">>> Negotiation"
      msgNegociation <- recv s 1024
      print $ S.unpack msgNegociation

      let d = negotiate $ S.unpack msgNegociation
      sendAll s $ S.pack d

      putStrLn ">>> Request"
      msgRequest <- recv s 1024
      print $ S.unpack msgRequest
      let (r, connection) = request $ S.unpack msgRequest
      print connection
      sendAll s $ S.pack r

      putStrLn ">>> Forward"
      runTCPClient (address connection) (port connection) $ \ss -> do
        _ <- forkIO $ stream s ss
        stream ss s
        putStrLn ">>> Completed"

-- from the "network-run" package.
