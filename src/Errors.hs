module Errors(RequestError(..)) where

import Data.Word (Word8)

data RequestError = NoResponseError String | ResponseError [Word8]
  deriving (Show, Eq)
