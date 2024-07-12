module Protocol (findNegotiationReturnCode, findPort, findIp, findCommand, Command (..)) where

import qualified Data.ByteString.Internal as BS (w2c)
import Data.List (intercalate)
import Data.Word (Word8)
import Numeric (showHex)
import Payload

data Command = Connect
  deriving (Eq, Show)

findNegotiationReturnCode :: NegotiationMessage -> Either Word8 Word8
findNegotiationReturnCode message =
  if 0 `elem` negotiationMethods message
    then Right 0
    else Left 255

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
  1 -> Right $ intercalate "." $ map show (requestAddress message)
  4 -> Right $ intercalate ":" $ map (\x -> showHex x "") $ doubleSize $ requestAddress message
  3 -> Right $ map BS.w2c (requestAddress message)
  _ -> Left 8
  
doubleSize :: [Word8] -> [Int]
doubleSize content = do
  let indexed = zip [0::Int ..] $ map fromIntegral $ content
  let weak = map snd $ filter (odd . fst) indexed
  let strong = map snd $ filter (even . fst) indexed
  map (\a -> ((fst a) * 256) + (snd a)) $ zip strong weak
