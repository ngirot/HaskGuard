module Lib (negotiate, request, errorResponse, Connection (..), RequestError (..)) where

import Data.Word (Word8)
import Errors
import Negotiation (generateNegotiationOutput2)
import Network.Socket
import Payload
import Request

data Connection = Connection
  { address :: String,
    port :: String
  }
  deriving (Show, Eq)

negotiate :: [Word8] -> Either RequestError [Word8]
negotiate payload = generateNegotiationOutput2 payload

request :: [Word8] -> ([Word8] -> Socket -> IO a) -> IO (Either RequestError a)
request payload onConnect = generateRequestOutput2 payload onConnect

errorResponse :: RequestMessage -> NetworkError -> [Word8]
errorResponse message err = case err of
  NameOrServiceNotKnown -> generateRequestErrorOutput message 4
