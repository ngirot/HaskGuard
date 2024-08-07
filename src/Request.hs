module Request (manageRequest) where

import Control.Arrow
import Data.Word (Word8)
import Errors
import Network
import Network.Socket
import Payload
import Protocol

manageRequest :: (String -> IO ()) -> [Word8] -> ([Word8] -> Socket -> IO a) -> IO (Either RequestError a)
manageRequest logger payload onConnect = do
  let parsedPayload = parseRequestInput payload
  case parsedPayload of
    Right message -> do
      let buildHost = buildIp message
      let port = buildPort message
      let command = buildCommand message

      case buildHost of
        Right host -> do
          logger $ "=== Resolved: " ++ host
          case command of
            Right Connect -> connectCommand message host port onConnect
            Left er -> pure $ Left $ er
        Left err -> pure $ Left err
    Left err -> pure $ Left $ NoResponseError err

buildPort :: RequestMessage -> String
buildPort = findPort

buildIp :: RequestMessage -> Either RequestError String
buildIp message = left mapError $ findIp message
  where
    mapError code = ResponseError $ generateRequestOutput message code

buildCommand :: RequestMessage -> Either RequestError Command
buildCommand message = left mapError $ findCommand message
  where
    mapError code = ResponseError $ generateRequestOutput message code

connectCommand :: RequestMessage -> String -> String -> ([Word8] -> Socket -> IO a) -> IO (Either RequestError a)
connectCommand message host port onConnect = do
  let resulPayload = generateRequestOutput message 0
  left mapError <$> runTCPClient host port (onConnect resulPayload)
  where
    mapError ConnectionRefused = ResponseError $ generateRequestOutput message 5
    mapError NameOrServiceNotKnown = ResponseError $ generateRequestOutput message 4
