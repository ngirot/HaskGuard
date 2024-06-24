module Payload (NegotiationMessage (..), RequestMessage (..), parseNegotiationInput, parseRequestInput, doubleSize, generateNegotiationOutput) where

import Data.Word (Word8)

data NegotiationMessage = NegotiationMessage
  { negotiationVersion :: Word8,
    negotiationMethods :: [Word8]
  }
  deriving (Show, Eq)

data RequestMessage = RequestMessage
  { requestVersion :: Word8,
    requestCommand :: Word8,
    requestAddressType :: Word8,
    requestAddress :: [Word8],
    requestPort :: [Word8]
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

generateNegotiationOutput :: NegotiationMessage -> Word8 -> [Word8]
generateNegotiationOutput message code = [negotiationVersion message, code]

parseRequestInput :: [Word8] -> Either String RequestMessage
parseRequestInput payload = do
  let version = extractVersion payload
  let command = extractCommand payload
  let addressType = extractAddressType payload
  let address = addressType >>= extractAddress payload
  let port = extractPort payload
  RequestMessage version <$> command <*> addressType <*> address <*> port
  where
    extractVersion p = p !! 0
    extractCommand p =
      if length p < 2
        then Left "Invalid payload size"
        else Right $ p !! 1
    extractAddressType p = Right $ p !! 3
    extractPort p = Right $ drop ((length p) - 2) p
    extractAddress p typ = case typ of
      1 ->
        if length payload == 10
          then Right $ take (length p -2 - 4) $ drop 4 p
          else Left "Invalid payload size"
      4 ->
        if length payload == 22
          then Right $ take (length p -2 - 4) $ drop 4 p
          else Left "Invalid payload size"
      _ ->
        if length payload > 7
          then Right $ take (length p -2 - 4) $ drop 4 p
          else Left "Invalid payload size"

doubleSize :: [Word8] -> [Int]
doubleSize content = do
  let indexed = zip [0 ..] $ map fromIntegral $ content
  let weak = map snd $ filter (odd . fst) indexed
  let strong = map snd $ filter (even . fst) indexed
  map (\a -> ((fst a) * 256) + (snd a)) $ zip strong weak
