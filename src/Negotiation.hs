module Negotiation (generateNegotiationOutput2) where

import Control.Arrow (left)
import Data.Word (Word8)
import Errors
import Payload
import Protocol

generateNegotiationOutput2 :: [Word8] -> Either RequestError [Word8]
generateNegotiationOutput2 payload = do
  let code = input >>= generateCode
  generatePayload <$> input <*> code
  where
    input = left (\message -> NoResponseError message) $ parseNegotiationInput payload
    generateCode i = left (\code -> ResponseError $ generateNegotiationOutput i code) $ findNegotiationReturnCode i
    generatePayload i code = generateNegotiationOutput i code
