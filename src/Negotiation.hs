module Negotiation (manageNegotiation, NegotiationResult (..)) where

import Config
import Control.Arrow (left)
import Data.Word (Word8)
import Errors
import Payload
import Protocol

data NegotiationResult = NoAuthenticationResult [Word8] | UsernamePasswordResult [Word8] | NegotiationError RequestError

manageNegotiation :: AuthenticationConfiguration -> [Word8] -> NegotiationResult
manageNegotiation conf payload = do
  case input of
    Left err -> NegotiationError err
    Right i -> do
      let code = generateCode i
      let p = generatePayload i <$> code
      case p of
        Left err -> NegotiationError err
        Right pl ->
          if (returnCode conf i) == Right 2
            then UsernamePasswordResult pl
            else NoAuthenticationResult pl
  where
    returnCode i = findNegotiationReturnCode i
    input = left NoResponseError $ parseNegotiationInput payload
    generateCode i = left (\code -> ResponseError $ generateNegotiationOutput i code) $ returnCode conf i
    generatePayload i code = generateNegotiationOutput i code
