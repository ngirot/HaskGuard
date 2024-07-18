module Lib (serve) where

import Authentication
import Config
import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString as S
import Data.Either
import Data.UUID
import Data.Word (Word8)
import Errors
import Negotiation (NegotiationResult (..), manageNegotiation)
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
    talk sock clientAddr = do
      uuid <- newUUID
      onConnectionReceived logger uuid sock clientAddr

onConnectionReceived :: (String -> IO ()) -> UUID -> Socket -> SockAddr -> IO ()
onConnectionReceived logger requestId sock clientAddr = do
  requestIdLogger $ "+++ Opened from " ++ show clientAddr
  msgNegotiation <- receiveData sock

  let negotiation = manageNegotiation msgNegotiation
  case negotiation of
    NoAuthenticationResult d -> afterNegotiation sock d
    UsernamePasswordResult d -> authenticate sock d
    NegotiationError (NoResponseError err) -> requestIdLogger $ "--- Closed with Error: " ++ err
    NegotiationError (ResponseError response) -> sendData sock response
  where
    requestIdLogger msg = logger $ "[" ++ show requestId ++ "] " ++ msg
    sendData = logSender requestIdLogger
    receiveData = logReceiver requestIdLogger
    authenticate s content = do
      sendData s content
      authPayload <- receiveData s
      let authenResult = manageAuthentication authPayload
      case authenResult of
        Right ppp -> afterNegotiation s ppp
        Left (NoResponseError err) -> requestIdLogger $ "--- Closed: " ++ err
        Left (ResponseError response) -> do
          sendData s response
          requestIdLogger "--- Closed"

    afterNegotiation s content = do
      sendData s content

      msgRequest <- receiveData s
      req <- manageRequest requestIdLogger msgRequest (onConnect s)
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

startOne :: ServerConfiguration -> (Socket -> SockAddr -> IO ()) -> IpType -> IO ((Async (Either String ()), MVar (Either String String)))
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
