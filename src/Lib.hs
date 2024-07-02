module Lib (negotiate, request, errorResponse, Connection (..), RequestError (..)) where

import Data.Word (Word8)
import Errors
import Negotiation (generateNegotiationOutput2)
import Payload
import Request (buildIp, buildPort, generateErrorOutput, generateRequestOutput)

data Connection = Connection
  { address :: String,
    port :: String
  }
  deriving (Show, Eq)

negotiate :: [Word8] -> Either RequestError [Word8]
negotiate payload = generateNegotiationOutput2 payload

request :: [Word8] -> Either RequestError ([Word8], Connection, RequestMessage)
request payload = do
  let message = parseRequestInput payload
  case message of
    Right m -> do
      let ip = buildIp m
      let connection = (\i -> Connection i (buildPort m)) <$> ip

      let resulPayload = generateRequestOutput m
      (\c -> (resulPayload, c, m)) <$> connection
    Left s -> Left $ NoResponseError s

errorResponse :: RequestMessage -> NetworkError -> [Word8]
errorResponse message err = case err of
  NameOrServiceNotKnown -> generateErrorOutput message 4
