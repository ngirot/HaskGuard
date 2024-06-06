module Negotiation(parseNegotiationInput, generateNegotiationOutput) where

import qualified Data.ByteString as S

data SocksVersion = V5 | V4 | UNKNOWN
  deriving (Enum, Eq, Show)

data AuthenticationMethod = NO_AUTHENTICATION | GSSAPI | USERNAME_PASSWORD | IANA_ASSIGNED | RESERVED | NO_ACCEPTABLE
  deriving (Enum, Eq, Show)

data NegotiationMessage = NegotiationMessage
  { version :: SocksVersion,
    method :: [AuthenticationMethod],
    tmp :: Int
  }
  deriving (Show, Eq)

parseNegotiationInput :: S.ByteString -> NegotiationMessage
parseNegotiationInput payload = do
  let unpacked = S.unpack payload

  let v = extractVersion $ Just $ unpacked!!0
  let numberOfMethods = extractNumberOfMethods $ Just $ unpacked!!1
  let methods = map extractMethod $ map (\x -> Just $ unpacked!!x) [2..(1 + numberOfMethods)]

  let message = NegotiationMessage v methods numberOfMethods
  message
  where
    extractVersion byte = case byte of
      Just 5 -> V5
      Just 4 -> V4
      _ -> UNKNOWN
    extractNumberOfMethods byte = case byte of
      Just a -> fromIntegral a :: Int
      _ -> 0 :: Int
    extractMethod byte = case byte of
      Just 0 -> NO_AUTHENTICATION
      Just 1 -> GSSAPI
      Just 2 -> USERNAME_PASSWORD
      Just x | x >= 3 && x <= 127 -> IANA_ASSIGNED
      Just x | x >= 128 && x <= 254 -> RESERVED
      _ -> NO_ACCEPTABLE

generateNegotiationOutput :: NegotiationMessage -> S.ByteString
generateNegotiationOutput _ = S.pack [5,0]