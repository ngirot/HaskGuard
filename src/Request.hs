module Request (buildPort, buildIp, parseRequestInput, generateRequestOutput) where

import Data.List (intercalate)
import Data.Word (Word8)
import Numeric (showHex)
import Payload

data RequestMessage = RequestMessage
  { version :: Word8,
    command :: Word8,
    addressType :: Word8,
    address :: [Word8],
    port :: [Word8]
  }
  deriving (Show, Eq)

buildPort :: RequestMessage -> String
-- buildPort message = show $ (((p !! 0) * 255) + p !! 1)
buildPort message = show $ (strong * 256) + weak
  where
    weak = fromIntegral $ p !! 1
    strong = fromIntegral $ p !! 0
    p = port message

buildIp :: RequestMessage -> String
buildIp message = case (addressType message) of
  1 -> intercalate "." $ map show (address message)
  4 -> intercalate ":" $ map (\x -> showHex x "") $ doubleSize $ address message

parseRequestInput :: [Word8] -> RequestMessage
parseRequestInput payload = do
  let version = extractVersion payload
  let command = extractCommand payload
  let addressType = extractAddressType payload
  let address = extractAddress payload
  let port = extractPort payload
  RequestMessage version command addressType address port
  where
    extractVersion p = p !! 0
    extractCommand p = p !! 1
    extractAddressType p = p !! 3
    extractPort p = drop ((length p) - 2) p
    extractAddress p = take (length p -2 - 4) $ drop 4 p

generateRequestOutput :: RequestMessage -> [Word8]
generateRequestOutput message = do
  [version message, 0, 0, (addressType message)] ++ (address message) ++ (port message)
