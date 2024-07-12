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
      req <- manageRequest msgRequest (onConnect s)
      case req of
        Right _ -> logger "Ok"
        Left (NoResponseError err) -> logger $ "Error: " ++ err
        Left (ResponseError response) -> sendData s response
    talk s = do
      logger ">>> Negotiation"
      msgNegociation <- receiveData s

      let nego = manageNegotiation msgNegociation
      case nego of
        Right d -> afterNego s d
        Left (NoResponseError err) -> logger $ "Error: " ++ err
        Left (ResponseError response) -> sendData s response
    sendData = logSender logger
    receiveData = logReceiver logger

normalizeListen :: IpType -> String -> String
normalizeListen IpV6 "0.0.0.0" = "::"
normalizeListen IpV4 "::" = "0.0.0.0"
normalizeListen _ l = l

startOne :: ServerConfiguration -> (Socket -> IO ()) -> IpType -> IO ((Async (Either String ()), MVar (Either String String)))
startOne configuration fn ipType = do
  mVar <- newEmptyMVar
  threadId <- async $ runTCPServer ipType (normalizeListen ipType $ scListen configuration) (show $ scPort configuration) (putMVar mVar) fn
  pure $ (threadId, mVar)

logSender :: (String -> IO ()) -> Socket -> [Word8] -> IO ()
logSender logger s dataToSend = do
  logger $ "<<< " ++ show dataToSend
  sendAll s $ S.pack dataToSend

logReceiver :: (String -> IO ()) -> Socket -> IO ([Word8])
logReceiver logger s = do
  d <- S.unpack <$> recv s 4096
  logger $ ">>> " ++ show d
  return d
