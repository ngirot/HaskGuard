module Lib (negotiate, request, errorResponse, Connection (..), RequestError (..)) where

import Data.Word (Word8)
import Errors
import Negotiation (generateNegotiationOutput2)
import Payload
import Request (buildIp, buildPort, generateErrorOutput, generateRequestOutput)
import Network.Socket
import Network
import Control.Arrow


data Connection = Connection
  { address :: String,
    port :: String
  }
  deriving (Show, Eq)

negotiate :: [Word8] -> Either RequestError [Word8]
negotiate payload = generateNegotiationOutput2 payload

request :: [Word8] -> ([Word8] -> Socket -> IO a) -> IO(Either RequestError a)
request payload onConnect = do
  let message = parseRequestInput payload
  case message of
    Right m -> do
      let ip = buildIp m
      let connection = (\i -> Connection i (buildPort m)) <$> ip

      let resulPayload = generateRequestOutput m
      case connection of
        Right conn -> fmap mapError $ runTCPClient (address conn) (port conn) (onConnect resulPayload)
        Left err -> pure $ Left err

    Left s -> pure $ Left $ NoResponseError s
  where
    mapError e = left (\_ -> NoResponseError "nope") e

errorResponse :: RequestMessage -> NetworkError -> [Word8]
errorResponse message err = case err of
  NameOrServiceNotKnown -> generateErrorOutput message 4
