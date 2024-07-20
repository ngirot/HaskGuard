module UserCsv (parseCsv, Credentials(..)) where

import qualified Data.ByteString.Internal as BS (c2w)
import qualified Data.ByteString.Lazy as L
import qualified Data.Csv as CSV
import qualified Data.Vector as V

data Credentials = Credentials
  { cLogin :: String,
    cPassword :: String
  }
  deriving (Show, Eq)

parseCsv :: String -> Either String [Credentials]
parseCsv content = do
  let csvContent = (CSV.decode CSV.NoHeader (L.pack $ fmap BS.c2w content))
  case csvContent of
    Right c -> Right $ map mapCredential $ V.toList c
    Left e -> Left $ "Unable to parse CSV: " ++ e
  where
    mapCredential (user, password) = Credentials {cLogin = user, cPassword = password}
