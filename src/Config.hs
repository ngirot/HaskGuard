{-# LANGUAGE OverloadedStrings #-}

module Config (parseConfig, GlobalConfiguration (..), AuthenticationConfiguration (..), ServerConfiguration (..), LogConfiguration (..), ApplicationConfiguration (..), defaultConfiguration, defaultLogConfiguration) where

import Data.Ini.Config
import Data.Maybe
import Data.Text (Text, pack, unpack)
import File
import System.Log.Logger
import UserCsv

data GlobalConfiguration = GlobalConfiguration
  { gcApplication :: ApplicationConfiguration,
    gcLog :: LogConfiguration
  }
  deriving (Eq, Show)

data ApplicationConfiguration = ApplicationConfiguration
  { acServer :: ServerConfiguration,
    acAuthentication :: AuthenticationConfiguration
  }
  deriving (Eq, Show)

data ServerConfiguration = ServerConfiguration
  { scListen :: String,
    scPort :: Int
  }
  deriving (Eq, Show)

data LogConfiguration = LogConfiguration
  { lcConsoleLevel :: Priority,
    lcFile :: Maybe FilePath,
    lcFileLevel :: Priority
  }
  deriving (Eq, Show)

data AuthenticationConfiguration = AuthenticationConfiguration
  { aucNoAuthentication :: Bool,
    aucUserPassword :: Bool,
    aucUsers :: [Credentials]
  }
  deriving (Eq, Show)

data AuthenticationSection = AuthenticationSection
  { ausNoAuthentication :: Bool,
    ausUserPassword :: Bool,
    ausUsername :: Maybe String,
    ausPassword :: Maybe String,
    ausUsersFile :: Maybe String
  }
  deriving (Eq, Show)

data ServerSection = ServerSection
  { ssListen :: String,
    ssPort :: Int
  }
  deriving (Eq, Show)

data LogSection = LogSection
  { lsConsoleLevel :: Priority,
    lsFile :: Maybe FilePath,
    lsFileLevel :: Priority
  }
  deriving (Eq, Show)

parseConfig :: String -> IO (Either String GlobalConfiguration)
parseConfig content = do
  let parsed = parseIniFile (pack content) configParser
  case parsed of
    Right config -> buildConf config
    Left err -> pure $ Left err
  where
    buildConf (server, logg, authentication) = buildConfiguration server logg authentication

configParser :: IniParser (ServerSection, LogSection, AuthenticationSection)
configParser = do
  serverSection <- fmap (fromMaybe defaultServerSection) $
    sectionMb "SERVER" $ do
      port <- fromMaybe (ssPort defaultServerSection) <$> fieldMbOf "port" number
      listen <- fromMaybe (pack $ ssListen defaultServerSection) <$> fieldMb "listen"
      return ServerSection {ssListen = unpack listen, ssPort = port}
  logSection <- fmap (fromMaybe defaultLogSection) $
    sectionMb "LOG" $ do
      consoleLevel <- fromMaybe (lsConsoleLevel defaultLogSection) <$> fieldMbOf "console-level" priority
      file <- fieldMb "file"
      fileLevel <- fromMaybe (lsConsoleLevel defaultLogSection) <$> fieldMbOf "file-level" priority
      return LogSection {lsConsoleLevel = consoleLevel, lsFile = unpack <$> file, lsFileLevel = fileLevel}
  authenticationSection <- fmap (fromMaybe defaultAuthenticationSection) $
    sectionMb "AUTHENTICATION" $ do
      noAuthentication <- fromMaybe (ausNoAuthentication defaultAuthenticationSection) <$> fieldMbOf "no-authentication" bool
      usernamePasswordAuthentication <- fromMaybe (ausUserPassword defaultAuthenticationSection) <$> fieldMbOf "user-password" bool
      username <- fieldMbOf "user" string
      password <- fieldMbOf "password" string
      usersFile <- fieldMbOf "users-file" string
      return AuthenticationSection {ausNoAuthentication = noAuthentication, ausUserPassword = usernamePasswordAuthentication, ausUsername = username, ausPassword = password, ausUsersFile = usersFile}
  return (serverSection, logSection, authenticationSection)

defaultConfiguration :: IO (Either String GlobalConfiguration)
defaultConfiguration = buildConfiguration defaultServerSection defaultLogSection defaultAuthenticationSection

defaultLogConfiguration :: LogConfiguration
defaultLogConfiguration = buildLogConfiguration defaultLogSection

defaultServerSection :: ServerSection
defaultServerSection = ServerSection {ssListen = "0.0.0.0", ssPort = 3128}

defaultLogSection :: LogSection
defaultLogSection = LogSection {lsConsoleLevel = INFO, lsFile = Nothing, lsFileLevel = INFO}

defaultAuthenticationSection :: AuthenticationSection
defaultAuthenticationSection = AuthenticationSection {ausNoAuthentication = True, ausUserPassword = False, ausUsername = Nothing, ausPassword = Nothing, ausUsersFile = Nothing}

buildConfiguration :: ServerSection -> LogSection -> AuthenticationSection -> IO (Either String GlobalConfiguration)
buildConfiguration server logg authentication = do
  applicationConfiguration <- buildApplicationConfiguration server authentication
  return $ case applicationConfiguration of
    Right c -> Right $ GlobalConfiguration {gcApplication = c, gcLog = buildLogConfiguration logg}
    Left err -> Left err

buildAuthenticationConfiguration :: AuthenticationSection -> IO (Either String AuthenticationConfiguration)
buildAuthenticationConfiguration authentication = do
  case ausUsersFile authentication of
    Nothing -> return $ Right $ buildConf []
    Just file -> do
      loaded <- loadUsers file
      case loaded of
        Right users -> return $ Right $ buildConf users
        Left err -> return $ Left err
  where
    buildConf list = do
      let userFromFile = userToList (ausUsername authentication) (ausPassword authentication)
      AuthenticationConfiguration
        { aucNoAuthentication = ausNoAuthentication authentication,
          aucUserPassword = ausUserPassword authentication,
          aucUsers = list ++ userFromFile
        }

buildApplicationConfiguration :: ServerSection -> AuthenticationSection -> IO (Either String ApplicationConfiguration)
buildApplicationConfiguration server authentication = do
  authConf <- buildAuthenticationConfiguration authentication
  return $ case authConf of
    Right c ->
      Right $
        ApplicationConfiguration
          { acServer = buildServerConfiguration server,
            acAuthentication = c
          }
    Left err -> Left err

buildLogConfiguration :: LogSection -> LogConfiguration
buildLogConfiguration logg =
  LogConfiguration
    { lcConsoleLevel = lsConsoleLevel logg,
      lcFile = lsFile logg,
      lcFileLevel = lsFileLevel logg
    }

buildServerConfiguration :: ServerSection -> ServerConfiguration
buildServerConfiguration server =
  ServerConfiguration
    { scListen = ssListen server,
      scPort = ssPort server
    }

priority :: Text -> Either String Priority
priority v = case v of
  "DEBUG" -> Right DEBUG
  "INFO" -> Right INFO
  "ERROR" -> Right ERROR
  _ -> Left $ show v ++ " is not a valid level (accepted are DEBUG, INFO and ERROR)"

bool :: Text -> Either String Bool
bool v = case v of
  "True" -> Right True
  "False" -> Right False
  _ -> Left $ show v ++ " is not a valid boolean (accepted are True and False)"

loadUsers :: FilePath -> IO (Either String [Credentials])
loadUsers fileName = do
  content <- loadFileContent fileName
  case content of
    Right t -> pure $ case parseCsv t of
      Right b -> Right b
      Left err -> Left $ "Bad user file: " ++ err
    Left err -> pure $ Left $ "Error loading file: " ++ show err

userToList :: Maybe String -> Maybe String -> [Credentials]
userToList (Just user) (Just password) = [Credentials user password]
userToList _ _ = []