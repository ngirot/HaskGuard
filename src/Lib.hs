module Lib (negotiate, request, Connection (..), RequestError (..)) where

import Data.Word (Word8)
import Errors
import Negotiation (generateNegotiationOutput, parseNegotiationInput)
import Request (buildIp, buildPort, generateRequestOutput, parseRequestInput)

data Connection = Connection
  { address :: String,
    port :: String
  }
  deriving (Show, Eq)

negotiate :: [Word8] -> Either String [Word8]
negotiate payload =
  generateNegotiationOutput <$> parseNegotiationInput payload

request :: [Word8] -> Either RequestError ([Word8], Connection)
request payload = do
  let message = parseRequestInput payload
  case message of
    Right m -> do
      let ip = buildIp m
      let connection = (\i -> Connection i (buildPort m)) <$> ip

      let resulPayload = generateRequestOutput m
      (\c -> (resulPayload, c)) <$> connection
    Left s -> Left $ NoResponseError s
