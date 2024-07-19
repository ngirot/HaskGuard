module Protocol (findNegotiationReturnCode, findPort, findIp, findCommand, Command (..)) where

import Config
import qualified Data.ByteString.Internal as BS (w2c)
import Data.List (intercalate, intersect)
import Data.Word (Word8)
import Numeric (showHex)
import Payload

data Command = Connect
  deriving (Eq, Show)

findNegotiationReturnCode :: AuthenticationConfiguration -> NegotiationMessage -> Either Word8 Word8
findNegotiationReturnCode config message = do
  let commons = intersect allowed (negotiationMethods message)
  if length commons > 0
    then Right $ head commons
    else Left 255
  where
    allowed = (filter (\_ -> aucNoAuthentication config) [0]) ++ (filter (\_ -> aucUserPassword config) [2])

findPort :: RequestMessage -> String
findPort message = show $ (strong * 256 :: Int) + weak
  where
    portBytes = requestPort message
    weak = fromIntegral $ portBytes !! 1
    strong = fromIntegral $ portBytes !! 0

findCommand :: RequestMessage -> Either Word8 Command
findCommand message = case (requestCommand message) of
  1 -> Right Connect
  _ -> Left 7

findIp :: RequestMessage -> Either Word8 String
findIp message = case requestAddressType message of
  1 -> Right $ intercalate "." $ map show $ requestAddress message
  4 -> Right $ intercalate ":" $ map (\x -> showHex x "") $ doubleSize $ requestAddress message
  3 -> Right $ map BS.w2c $ requestAddress message
  _ -> Left 8

doubleSize :: [Word8] -> [Int]
doubleSize content = do
  let indexed = zip [0 :: Int ..] $ map fromIntegral $ content
  let weak = snd <$> filter (odd . fst) indexed
  let strong = snd <$> filter (even . fst) indexed
  map (\a -> ((fst a) * 256) + (snd a)) $ zip strong weak
