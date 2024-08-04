module Step (Step(..), processStep) where

import Authentication
import Config
import Control.Concurrent
import qualified Data.ByteString as S
import Data.Word (Word8)
import Errors
import Negotiation (NegotiationResult (..), manageNegotiation)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Request
import Streaming

data Step = OPEN String | NEGOTIATION | AUTHENTICATION | CONNECT | CLOSE_OK | CLOSE_ERROR RequestError

processStep :: Step -> AuthenticationConfiguration -> Socket -> (String -> IO ()) -> IO (Maybe Step)
processStep (OPEN clientAddr) _ _ logger = do
  logger $ "+++ Opened from " ++ show clientAddr
  return $ Just NEGOTIATION
processStep NEGOTIATION authConf s logger = do
  msgNegotiation <- logReceiver logger s
  let negotiation = manageNegotiation authConf msgNegotiation
  case negotiation of
    NoAuthenticationResult payload -> do
      logSender logger s payload
      pure $ Just CONNECT
    UsernamePasswordResult payload -> do
      logSender logger s payload
      pure $ Just AUTHENTICATION
    NegotiationError e -> pure $ Just $ CLOSE_ERROR e
processStep CONNECT _ s logger = do
  msgRequest <- logReceiver logger s
  req <- manageRequest logger msgRequest (onConnect s)
  case req of
    Right _ -> pure $ Just CLOSE_OK
    Left e -> pure $ Just $ CLOSE_ERROR e
  where
    onConnect socketSource content socketDestination = do
      logSender logger socketSource content
      _ <- forkIO $ stream (\msg -> logger $ "|>>" ++ msg) socketSource socketDestination
      stream (\msg -> logger $ "|<<" ++ msg) socketDestination socketSource
processStep AUTHENTICATION authConf s logger = do
  authPayload <- logReceiver logger s
  authResult <- manageAuthentication authConf logger authPayload
  case authResult of
    Right payload -> do
      logSender logger s payload
      return $ Just CONNECT
    Left e -> return $ Just $ CLOSE_ERROR e
processStep CLOSE_OK _ _ logger = do
  logger "--- Closed"
  return Nothing
processStep (CLOSE_ERROR (NoResponseError err)) _ _ logger = do
  logger $ "--- Closed with error: " ++ err
  return Nothing
processStep (CLOSE_ERROR (ResponseError payload)) _ s logger = do
  logSender logger s payload
  logger $ "--- Closed with error payload"
  return Nothing

logSender :: (String -> IO ()) -> Socket -> [Word8] -> IO ()
logSender logger sock dataToSend = do
  logger $ "<<< " ++ show dataToSend
  sendAll sock $ S.pack dataToSend

logReceiver :: (String -> IO ()) -> Socket -> IO ([Word8])
logReceiver logger sock = do
  content <- S.unpack <$> recv sock 4096
  logger $ ">>> " ++ show content
  return content
