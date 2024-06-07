module Lib (negotiate, request, address, port) where

import qualified Data.ByteString as S
import Negotiation (generateNegotiationOutput, parseNegotiationInput)
import Request (buildIp, buildPort, generateRequestOutput, parseRequestInput)

data Connection = Connection
  { address :: String,
    port :: String
  }
  deriving (Show, Eq)

negotiate :: S.ByteString -> S.ByteString
negotiate payload =
  S.pack $ generateNegotiationOutput $ parseNegotiationInput (S.unpack payload)

request :: S.ByteString -> (S.ByteString, Connection)
request payload = do
  let message = parseRequestInput (S.unpack payload)
  let connection = Connection (buildIp message) (buildPort message)
  let resulPayload = S.pack $ generateRequestOutput $ message
  (resulPayload, connection)
