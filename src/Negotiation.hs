module Negotiation (parseNegotiationInput, generateNegotiationOutput) where

import Data.Word (Word8)

data NegotiationMessage = NegotiationMessage
  { version :: Word8,
    methods :: [Word8]
  }
  deriving (Show, Eq)

parseNegotiationInput :: [Word8] -> NegotiationMessage
parseNegotiationInput payload = do
  let version = extractVersion payload
  let numberOfMethods = extractNumberOfMethods payload
  let methods = extractMethods payload numberOfMethods

  let message = NegotiationMessage version methods
  message
  where
    extractVersion p = p !! 0
    extractNumberOfMethods p = fromIntegral (p !! 1)
    extractMethods p size = map (\x -> p !! x) [2 .. (1 + size)]

generateNegotiationOutput :: NegotiationMessage -> [Word8]
generateNegotiationOutput message = do
  if elem 0 (methods message)
    then [version message, 0]
    else [version message, 255]
