module Lib (serve) where

import Config
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString as S
import Data.Either
import Errors
import Negotiation (manageNegotiation)
import Network
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Request
import Streaming (stream)

serve :: ServerConfiguration -> (String -> IO ()) -> ([String] -> [String] -> IO ()) -> IO ()
serve configuration logger onStartup = do
  threads <- mapM (startOne configuration talk) [IpV4, IpV6]

  hostsStarted <- mapM takeMVar $ fmap snd threads
  onStartup (lefts hostsStarted) (rights hostsStarted)

  mapM_ wait $ fmap fst threads
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
normalizeListen _ l = l

--startOne :: ServerConfiguration -> (Socket -> IO ()) -> IpType -> IO ((IO(Async (Either String ()))), IO(MVar (Either String String)))
-- startOne :: ServerConfiguration -> (Socket -> IO ()) -> IpType -> IO(MVar (Either String String))
startOne :: ServerConfiguration -> (Socket -> IO ()) -> IpType -> IO ((Async (Either String ()), MVar (Either String String)))
startOne configuration fn ipType = do
  mVar <- newEmptyMVar
  threadId <- async $ runTCPServer ipType (normalizeListen ipType $ scListen configuration) (show $ scPort configuration) (putMVar mVar) fn
  -- pure mVar
  -- pure threadId
  pure $ (threadId, mVar)
