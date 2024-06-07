module Lib (negotiate, request) where

import qualified Data.ByteString as S
import Negotiation (generateNegotiationOutput, parseNegotiationInput)
import Request (generateRequestOutput, parseRequestInput)

negotiate :: S.ByteString -> S.ByteString
negotiate payload =
  S.pack $ generateNegotiationOutput $ parseNegotiationInput (S.unpack payload)

request :: S.ByteString -> S.ByteString
request payload =
  generateRequestOutput $ parseRequestInput payload
