module Negotiation (manageNegotiation, NegotiationResult (..)) where

import Control.Arrow (left)
import Data.Word (Word8)
import Errors
import Payload
import Protocol

data NegotiationResult = NoAuthenticationResult [Word8] | UsernamePasswordResult [Word8] | NegotiationError RequestError

manageNegotiation :: [Word8] -> NegotiationResult
manageNegotiation payload = do
  case input of
    Left err -> NegotiationError err
    Right i -> do
      let code = generateCode i
      let p = generatePayload i <$> code
      case p of
        Left err -> NegotiationError err
        Right pl ->
          if (returnCode i) == Right 2
            then UsernamePasswordResult pl
            else NoAuthenticationResult pl
  where
    returnCode i = findNegotiationReturnCode i
    input = left NoResponseError $ parseNegotiationInput payload
    generateCode i = left (\code -> ResponseError $ generateNegotiationOutput i code) $ returnCode i
    generatePayload i code = generateNegotiationOutput i code
