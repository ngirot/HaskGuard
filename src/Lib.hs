module Lib (negotiate, request, address, port) where

import Negotiation (generateNegotiationOutput, parseNegotiationInput)
import Request (buildIp, buildPort, generateRequestOutput, parseRequestInput)
import Data.Word (Word8)

data Connection = Connection
  { address :: String,
    port :: String
  }
  deriving (Show, Eq)

negotiate :: [Word8] -> [Word8]
negotiate payload =
  generateNegotiationOutput $ parseNegotiationInput payload

request :: [Word8] -> ([Word8], Connection)
request payload = do
  let message = parseRequestInput payload
  let connection = Connection (buildIp message) (buildPort message)
  let resulPayload = generateRequestOutput message
  (resulPayload, connection)
