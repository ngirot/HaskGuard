module Payload
  ( NegotiationMessage (..),
    RequestMessage (..),
    UserPasswordMessage (..),
    parseNegotiationInput,
    parseRequestInput,
    generateNegotiationOutput,
    generateRequestOutput,
    parseUserPasswordInput,
    generateUserPasswordOutput,
  )
where

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

data UserPasswordMessage = UserPasswordMessage
  { upVersion :: Word8,
    upUser :: [Word8],
    upPassword :: [Word8]
  }
  deriving (Show, Eq)

parseNegotiationInput :: [Word8] -> Either String NegotiationMessage
parseNegotiationInput payload = do
  let version = extractVersion payload
  let methods = extractNumberOfMethods payload >>= extractMethods payload

  NegotiationMessage version <$> methods
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
      3 ->
        if length payload >= 7 && isVariablePayloadSizeValid
          then Right $ take (length p -3 - 4) $ drop 5 p
          else Left "Invalid payload size"
      _ ->
        if length payload >= 7
          then Right $ take (length p -2 - 4) $ drop 4 p
          else Left "Invalid payload size"

    isVariablePayloadSizeValid = do
      let size = fromIntegral $ payload !! 4
      let isSizeConsistent = size + 7 == length payload
      let isSizeNotEmpty = size /= 0
      isSizeConsistent && isSizeNotEmpty

generateRequestOutput :: RequestMessage -> Word8 -> [Word8]
generateRequestOutput message code =
  build $ requestAddressType message
  where
    build addrType = case addrType of
      3 -> [requestVersion message, code, 0, addrType, (fromIntegral $ length $ requestAddress message)] ++ (requestAddress message) ++ (requestPort message)
      _ -> [requestVersion message, code, 0, addrType] ++ (requestAddress message) ++ (requestPort message)

parseUserPasswordInput :: [Word8] -> Either String UserPasswordMessage
parseUserPasswordInput payload = do
  if userNameSize + passwordSize + 3 /= length payload
    then Left $ "Invalid payload size"
    else
      if userNameSize == 0
        then Left "Invalid payload: no username given"
        else
          if passwordSize == 0
            then Left "Invalid payload: no password given"
            else Right $ UserPasswordMessage version username password
  where
    version = payload !! 0
    userNameSize = fromIntegral $ payload !! 1
    passwordOffset = userNameSize + 2
    passwordSize =
      if passwordOffset < length payload
        then fromIntegral $ payload !! passwordOffset
        else 0
    username = take userNameSize $ drop 2 payload
    password = take passwordSize $ drop (3 + userNameSize) payload

generateUserPasswordOutput :: UserPasswordMessage -> Word8 -> [Word8]
generateUserPasswordOutput message code = [upVersion message, code]
