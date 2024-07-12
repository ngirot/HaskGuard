module Lib (serve) where

import Config
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString as S
import Data.Either
import Errors
import Negotiation (manageNegotiation)
import Network
import Network.Socket.ByteString (recv, sendAll)
import Request
import Streaming (stream)

serve :: ServerConfiguration -> (String -> IO ()) -> ([String] -> [String] -> IO ()) -> IO ()
serve configuration logger onStartup = do
  startedV4 <- newEmptyMVar
  startedV6 <- newEmptyMVar

  threadV4 <- async $ runTCPServer IpV4 (normalizeListen IpV4 $ scListen configuration) (show $ scPort configuration) (putMVar startedV4) talk
  threadV6 <- async $ runTCPServer IpV6 (normalizeListen IpV6 $ scListen configuration) (show $ scPort configuration) (putMVar startedV6) talk

  hostsStarted <- mapM takeMVar [startedV4, startedV6]
  onStartup (lefts hostsStarted) (rights hostsStarted)

  mapM_ wait [threadV4, threadV6]
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

normalizeListen :: IpType -> String -> String
normalizeListen IpV6 "0.0.0.0" = "::"
normalizeListen IpV4 "::" = "0.0.0.0"
normalizeListen _ listen = listen
