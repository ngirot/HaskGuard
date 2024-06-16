module Lib (negotiate, request, Connection (..)) where

import Data.Either
import Data.Word (Word8)
import Negotiation (generateNegotiationOutput, parseNegotiationInput)
import Request (RequestMessage (..), buildIp, buildPort, generateRequestOutput, parseRequestInput)

data Connection = Connection
  { address :: String,
    port :: String
  }
  deriving (Show, Eq)

negotiate :: [Word8] -> Either String [Word8]
negotiate payload =
  generateNegotiationOutput <$> parseNegotiationInput payload

request :: [Word8] -> Either String ([Word8], Connection)
request payload = do
  let message = parseRequestInput payload
  case message of
    Right m -> do
      let connection = Connection (buildIp m) (buildPort m)
      let resulPayload = generateRequestOutput m
      Right (resulPayload, connection)
    Left s -> Left s
