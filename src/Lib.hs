module Lib (serve) where

import Config
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString as S
import Data.Either
import Data.UUID
import Data.Word (Word8)
import Errors
import Negotiation (manageNegotiation)
import Network
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Request
import Streaming (stream)
import System.Random

serve :: ServerConfiguration -> (String -> IO ()) -> ([String] -> [String] -> IO ()) -> IO ()
serve configuration logger onStartup = do
  threads <- mapM (startOne configuration talk) [IpV4, IpV6]

  hostsStarted <- mapM takeMVar $ snd <$> threads
  onStartup (lefts hostsStarted) (rights hostsStarted)

  mapM_ wait $ fst <$> threads
  where
    talk sock = do
      uuid <- newUUID
      onConnectionReceived logger uuid sock

onConnectionReceived :: (String -> IO ()) -> UUID -> Socket -> IO ()
onConnectionReceived logger requestId sock = do
  requestIdLogger "+++ Opened"
  msgNegotiation <- receiveData sock

  let negotiation = manageNegotiation msgNegotiation
  case negotiation of
    Right d -> afterNegotiation sock d
    Left (NoResponseError err) -> requestIdLogger $ "Error: " ++ err
    Left (ResponseError response) -> sendData sock response
  where
    requestIdLogger msg = logger $ "[" ++ show requestId ++ "] " ++ msg
    sendData = logSender requestIdLogger
    receiveData = logReceiver requestIdLogger
    afterNegotiation s content = do
      sendData s content

      msgRequest <- receiveData s
      req <- manageRequest msgRequest (onConnect s)
      case req of
        Right _ -> requestIdLogger "--- Closed"
        Left (NoResponseError err) -> requestIdLogger $ "--- Closed: " ++ err
        Left (ResponseError response) -> do
          sendData s response
          requestIdLogger "--- Closed"
    onConnect socketSource content socketDestination = do
      sendData socketSource content
      _ <- forkIO $ stream (\msg -> requestIdLogger $ "|>>" ++ msg) socketSource socketDestination
      stream (\msg -> requestIdLogger $ "|<<" ++ msg) socketDestination socketSource

normalizeListen :: IpType -> String -> String
normalizeListen IpV6 "0.0.0.0" = "::"
normalizeListen IpV4 "::" = "0.0.0.0"
normalizeListen _ host = host

startOne :: ServerConfiguration -> (Socket -> IO ()) -> IpType -> IO ((Async (Either String ()), MVar (Either String String)))
startOne configuration fn ipType = do
  mVar <- newEmptyMVar
  threadId <- async $ runTCPServer ipType (normalizeListen ipType $ scListen configuration) (show $ scPort configuration) (putMVar mVar) fn
  pure $ (threadId, mVar)

logSender :: (String -> IO ()) -> Socket -> [Word8] -> IO ()
logSender logger sock dataToSend = do
  logger $ "<<< " ++ show dataToSend
  sendAll sock $ S.pack dataToSend

logReceiver :: (String -> IO ()) -> Socket -> IO ([Word8])
logReceiver logger sock = do
  content <- S.unpack <$> recv sock 4096
  logger $ ">>> " ++ show content
  return content

newUUID :: IO UUID
newUUID = randomIO
