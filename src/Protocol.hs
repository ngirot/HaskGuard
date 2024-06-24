module Protocol (findNegotiationReturnCode) where

import Payload
import Data.Word (Word8)

findNegotiationReturnCode :: NegotiationMessage -> Either Word8 Word8
findNegotiationReturnCode message =
  if elem 0 (negotiationMethods message)
    then Right 0
    else Left 255
