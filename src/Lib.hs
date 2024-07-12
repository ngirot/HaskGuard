module Lib (serve) where

import Config
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString as S
import Data.Either
import Data.Word (Word8)
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

  hostsStarted <- mapM takeMVar $ snd <$> threads
  onStartup (lefts hostsStarted) (rights hostsStarted)

  mapM_ wait $ fst <$> threads
  where
    onConnect s r ss = do
      sendData s r
      _ <- forkIO $ stream s ss
      stream ss s
      logger ">>> Completed"

    afterNego s d = do
      sendData s d

      logger ">>> Request"
      msgRequest <- receiveData s
      logger $ show  msgRequest
      req <- manageRequest msgRequest (onConnect s)
      case req of
        Right _ -> logger "Ok"
        Left (NoResponseError err) -> logger $ "Error: " ++ err
        Left (ResponseError response) -> sendData s response
    talk s = do
      logger ">>> Negotiation"
      msgNegociation <- receiveData s
      logger $ show msgNegociation

      let nego = manageNegotiation msgNegociation
      case nego of
        Right d -> afterNego s d
        Left (NoResponseError err) -> logger $ "Error: " ++ err
        Left (ResponseError response) -> sendData s response

normalizeListen :: IpType -> String -> String
normalizeListen IpV6 "0.0.0.0" = "::"
normalizeListen IpV4 "::" = "0.0.0.0"
normalizeListen _ l = l

startOne :: ServerConfiguration -> (Socket -> IO ()) -> IpType -> IO ((Async (Either String ()), MVar (Either String String)))
startOne configuration fn ipType = do
  mVar <- newEmptyMVar
  threadId <- async $ runTCPServer ipType (normalizeListen ipType $ scListen configuration) (show $ scPort configuration) (putMVar mVar) fn
  pure $ (threadId, mVar)

sendData :: Socket -> [Word8] -> IO ()
sendData s dataToSend = sendAll s $ S.pack dataToSend

receiveData :: Socket -> IO ([Word8])
receiveData s = S.unpack <$> recv s 4096
