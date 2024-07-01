module Lib (negotiate, request, errorResponse, Connection (..), RequestError (..)) where

import Data.Word (Word8)
import Errors
import Negotiation (generateNegotiationOutput2)
import Payload

data Connection = Connection
  { address :: String,
    port :: String
  }
  deriving (Show, Eq)

negotiate :: [Word8] -> Either RequestError [Word8]
negotiate payload = generateNegotiationOutput2 payload

request :: [Word8] -> Either RequestError ([Word8], Connection, RequestMessage)
request payload = do
  Left $ NoResponseError "Not implements"

errorResponse :: RequestMessage -> NetworkError -> [Word8]
errorResponse message err = case err of
  NameOrServiceNotKnown -> []
