module Errors (RequestError (..), NetworkError (..)) where

import Data.Word (Word8)

data RequestError = NoResponseError String | ResponseError [Word8]
  deriving (Show, Eq)

data NetworkError = NameOrServiceNotKnown | ConnectionRefused
  deriving (Show, Eq)
