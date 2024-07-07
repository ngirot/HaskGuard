module Lib (negotiate, request) where

import Data.Word (Word8)
import Errors
import Negotiation (generateNegotiationOutput2)
import Network.Socket
import Request

negotiate :: [Word8] -> Either RequestError [Word8]
negotiate payload = generateNegotiationOutput2 payload

request :: [Word8] -> ([Word8] -> Socket -> IO a) -> IO (Either RequestError a)
request payload onConnect = generateRequestOutput2 payload onConnect
