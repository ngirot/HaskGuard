module Negotiation (parseNegotiationInput, generateNegotiationOutput) where

import Data.Word (Word8)

data NegotiationMessage = NegotiationMessage
  { version :: Word8,
    methods :: [Word8]
  }
  deriving (Show, Eq)

parseNegotiationInput :: [Word8] -> Either String NegotiationMessage
parseNegotiationInput payload = do
  let version = extractVersion payload
  let methods = extractNumberOfMethods payload >>= extractMethods payload

  let message = NegotiationMessage version <$> methods
  message
  where
    extractVersion p = p !! 0
    extractNumberOfMethods p =
      if length p > 1
        then Right $ fromIntegral (p !! 1)
        else Left "Payload has invalid size"
    extractMethods p size =
      if length p == size + 2
        then Right $ map (\x -> p !! x) [2 .. (1 + size)]
        else Left "Payload has invalid size"

generateNegotiationOutput :: NegotiationMessage -> [Word8]
generateNegotiationOutput message = do
  if elem 0 (methods message)
    then [version message, 0]
    else [version message, 255]