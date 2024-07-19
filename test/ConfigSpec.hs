{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Config
import Data.Ini.Config
import System.Log.Logger
import Test.Hspec

spec :: Spec
spec = do
  serverConfig
  logConfig
  authenticationConfig

serverConfig :: Spec
serverConfig =
  describe "Server configuration" $ do
    it "Should load complete server section" $ parse completeFile `shouldBe` (Right $ ServerConfiguration {scListen = "localhost", scPort = 80})
    it "Should set default 'port' value to '3128'" $ scPort <$> parse "" `shouldBe` Right 3128
    it "Should set default 'listen' value to listen everything" $ scListen <$> parse "" `shouldBe` Right "0.0.0.0"
    it "Should have an error when 'port' is not a valid number" $ parse invalidPort `shouldBe` Left "Line 2, in section \"SERVER\": Unable to parse \"test\" as a value of type Int"
  where
    parse fileContent = acServer <$> gcApplication <$> parseIniFile fileContent configParser
    completeFile = "[SERVER]\nport=80\nlisten=localhost\n"
    invalidPort = "[SERVER]\nport=test\n"

authenticationConfig :: Spec
authenticationConfig =
  describe "Authentication configuration" $ do
    it "Should load complete authentication section" $ parse completeFile `shouldBe` (Right $ AuthenticationConfiguration {aucNoAuthentication = False, aucUserPassword = True, aucUsername = Just "us", aucPassword = Just "pwd"})
    it "Should set default 'no authentication' to True" $ aucNoAuthentication <$> parse "" `shouldBe` Right True
    it "Should set default 'UsernamePassword authentication' to False" $ aucUserPassword <$> parse "" `shouldBe` Right False
    it "Should set default 'user' to Nothing" $ aucUsername <$> parse "" `shouldBe` Right Nothing
    it "Should set default 'password' to Nothing" $ aucPassword <$> parse "" `shouldBe` Right Nothing
    it "Should have an error when 'no authentication' is not a boolean" $ aucNoAuthentication <$> parse invalidNoAuthenticationBoolean `shouldBe` Left "Line 2, in section \"AUTHENTICATION\": \"invalid\" is not a valid boolean (accepted are True and False)"
    it "Should have an error when 'UsernamePassword authentication' is not a boolean" $ aucUserPassword <$> parse invalidUsernamePasswordBoolean `shouldBe` Left "Line 2, in section \"AUTHENTICATION\": \"8\" is not a valid boolean (accepted are True and False)"
  where
    parse fileContent = acAuthentication <$> gcApplication <$> parseIniFile fileContent configParser
    completeFile = "[AUTHENTICATION]\nno-authentication = False\nuser-password = True\nuser = us\npassword = pwd\n"
    invalidNoAuthenticationBoolean = "[AUTHENTICATION]\nno-authentication=invalid\n"
    invalidUsernamePasswordBoolean = "[AUTHENTICATION]\nuser-password=8\n"

logConfig :: Spec
logConfig =
  describe "Server configuration" $ do
    it "Should load complete server section" $ parse completeFile `shouldBe` (Right $ LogConfiguration {lcConsoleLevel = DEBUG, lcFile = Just "/tmp/haskguard.log", lcFileLevel = ERROR})
    it "Should set default 'console level' to 'INFO'" $ lcConsoleLevel <$> parse "" `shouldBe` Right INFO
    it "Should set default 'file' to nothing" $ lcFile <$> parse "" `shouldBe` Right Nothing
    it "Should set default 'file level' to 'INFO'" $ lcFileLevel <$> parse "" `shouldBe` Right INFO
    it "Should have an error when 'console level' is not a level" $ parse invalidConsoleLevel `shouldBe` Left "Line 2, in section \"LOG\": \"NOPE\" is not a valid level (accepted are DEBUG, INFO and ERROR)"
    it "Should have an error when 'file level' is not a level" $ parse invalidFileLevel `shouldBe` Left "Line 2, in section \"LOG\": \"FAKE\" is not a valid level (accepted are DEBUG, INFO and ERROR)"
  where
    parse fileContent = gcLog <$> parseIniFile fileContent configParser
    completeFile = "[LOG]\nconsole-level=DEBUG\nfile=/tmp/haskguard.log\nfile-level=ERROR\n"
    invalidConsoleLevel = "[LOG]\nconsole-level=NOPE\n"
    invalidFileLevel = "[LOG]\nfile-level=FAKE\n"
