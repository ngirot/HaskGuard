module Lib (serve) where

import Config
import Control.Concurrent
import qualified Data.ByteString as S
import Errors
import Negotiation (manageNegotiation)
import Network
import Network.Socket.ByteString (recv, sendAll)
import Request
import Streaming (stream)

serve :: ServerConfiguration -> (String -> IO()) -> IO() -> IO ()
serve configuration logger onStartup = do
  runTCPServer (Just $ scListen configuration) (show $ scPort configuration) onStartup talk
  where
    onConnect s r ss = do
      sendAll s $ S.pack r
      _ <- forkIO $ stream s ss
      stream ss s
      logger ">>> Completed"

    afterNego s d = do
      sendAll s $ S.pack d

      logger ">>> Request"
      msgRequest <- recv s 1024
      logger $ show $ S.unpack msgRequest
      req <- manageRequest (S.unpack msgRequest) (onConnect s)
      case req of
        Right _ -> logger "Ok"
        Left (NoResponseError err) -> logger $ "Error: " ++ err
        Left (ResponseError response) -> sendAll s $ S.pack response
    talk s = do
      logger ">>> Negotiation"
      msgNegociation <- recv s 1024
      logger $ show $ S.unpack msgNegociation

      let nego = manageNegotiation $ S.unpack msgNegociation
      case nego of
        Right d -> afterNego s d
        Left (NoResponseError err) -> logger $ "Error: " ++ err
        Left (ResponseError response) -> sendAll s $ S.pack response
