module Request (manageRequest) where

import Control.Arrow
import Data.Word (Word8)
import Errors
import Network
import Network.Socket
import Payload
import Protocol

manageRequest :: [Word8] -> ([Word8] -> Socket -> IO a) -> IO (Either RequestError a)
manageRequest payload onConnect = do
  let parsedPayload = parseRequestInput payload
  case parsedPayload of
    Right message -> do
      let buildHost = buildIp message
      let port = buildPort message

      case buildHost of
        Right host -> do
          let resulPayload = generateRequestSuccessOutput message
          left (mapError message) <$> runTCPClient host port (onConnect resulPayload)
        Left err -> pure $ Left err
    Left err -> pure $ Left $ NoResponseError err
  where
    mapError message _ = ResponseError $ generateRequestErrorOutput message 4

buildPort :: RequestMessage -> String
buildPort = findPort

buildIp :: RequestMessage -> Either RequestError String
buildIp message = left mapError $ findIp message
  where
    mapError code = ResponseError $ generateRequestOutput message code

generateRequestSuccessOutput :: RequestMessage -> [Word8]
generateRequestSuccessOutput message = generateRequestOutput message 0

generateRequestErrorOutput :: RequestMessage -> Word8 -> [Word8]
generateRequestErrorOutput message code = generateRequestOutput message code
