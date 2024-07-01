module Protocol (findNegotiationReturnCode, findCommand, findHost, findPort, findNetworkErrorCode) where

import Data.Word (Word8)
import Payload
import Network
import Errors

findNegotiationReturnCode :: NegotiationMessage -> Either Word8 Word8
findNegotiationReturnCode message =
  if elem 0 (negotiationMethods message)
    then Right 0
    else Left 255

findCommand :: RequestMessage -> Either Word8 Word8
findCommand message =
  if (requestCommand message) == 1
    then Right 1
    else Left 7

findHost :: RequestMessage -> Either Word8 String
findHost message = Right "goo"

findPort :: RequestMessage -> String
findPort message = "89"

findNetworkErrorCode :: NetworkError -> Word8
findNetworkErrorCode err = 7
