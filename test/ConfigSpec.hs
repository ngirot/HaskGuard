{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Config
import System.Log.Logger
import Test.Hspec
import UserCsv

spec :: Spec
spec = do
  serverConfig
  logConfig
  authenticationConfig

serverConfig :: Spec
serverConfig =
  describe "Server configuration" $ do
    it "Should load complete server section" $ do
      parsed <- parse completeFile
      parsed `shouldBe` (Right $ ServerConfiguration {scListen = "localhost", scPort = 80})
    it "Should set default 'port' value to '3128'" $ do
      parsed <- parse ""
      scPort <$> parsed `shouldBe` Right 3128
    it "Should set default 'listen' value to listen everything" $ do
      parsed <- parse ""
      scListen <$> parsed `shouldBe` Right "0.0.0.0"
    it "Should have an error when 'port' is not a valid number" $ do
      parsed <- parse invalidPort
      parsed `shouldBe` Left "Line 2, in section \"SERVER\": Unable to parse \"test\" as a value of type Int"
  where
    parse fileContent = do
      parsed <- parseConfig fileContent
      case parsed of
        Right x -> pure $ Right $ acServer $ gcApplication x
        Left x -> pure $ Left x
    completeFile = "[SERVER]\nport=80\nlisten=localhost\n"
    invalidPort = "[SERVER]\nport=test\n"

authenticationConfig :: Spec
authenticationConfig =
  describe "Authentication configuration" $ do
    it "Should load complete authentication section" $ do
      parsed <- parse completeFile
      parsed `shouldBe` (Right $ AuthenticationConfiguration {aucNoAuthentication = False, aucUserPassword = True, aucUsers = [Credentials "us" "pwd"]})
    it "Should set default 'no authentication' to True" $ do
      parsed <- parse ""
      aucNoAuthentication <$> parsed `shouldBe` Right True
    it "Should set default 'UsernamePassword authentication' to False" $ do
      parsed <- parse ""
      aucUserPassword <$> parsed `shouldBe` Right False
    it "Should set default 'user' and 'password' to nothing" $ do
      parsed <- parse ""
      aucUsers <$> parsed `shouldBe` Right []
    it "Should have an error when 'no authentication' is not a boolean" $ do
      parsed <- parse invalidNoAuthenticationBoolean
      aucNoAuthentication <$> parsed `shouldBe` Left "Line 2, in section \"AUTHENTICATION\": \"invalid\" is not a valid boolean (accepted are True and False)"
    it "Should have an error when 'UsernamePassword authentication' is not a boolean" $ do
      parsed <- parse invalidUsernamePasswordBoolean
      aucUserPassword <$> parsed `shouldBe` Left "Line 2, in section \"AUTHENTICATION\": \"8\" is not a valid boolean (accepted are True and False)"
    it "Should load user file" $ do
      parsed <- parse userFile
      aucUsers <$> parsed `shouldBe` Right [Credentials "user1" "password1", Credentials "user2" "password2"]
    it "Should fails if user file is invalid" $ do
      parsed <- parse invalidUserFile
      aucUsers <$> parsed `shouldBe` Left "Bad user file: Unable to parse CSV: parse error (Failed reading: conversion error: cannot unpack array of length 3 into a pair. Input record: [\"bad-user\",\"extra\",\"bad-password\"]) at \"\""
  where
    parse fileContent = do
      parsed <- parseConfig fileContent
      case parsed of
        Right x -> pure $ Right $ acAuthentication $ gcApplication x
        Left x -> pure $ Left x
    completeFile = "[AUTHENTICATION]\nno-authentication = False\nuser-password = True\nuser = us\npassword = pwd\n"
    invalidNoAuthenticationBoolean = "[AUTHENTICATION]\nno-authentication=invalid\n"
    invalidUsernamePasswordBoolean = "[AUTHENTICATION]\nuser-password=8\n"
    userFile = "[AUTHENTICATION]\nuser-password=True\nusers-file=test/users.csv\n"
    invalidUserFile = "[AUTHENTICATION]\nuser-password=True\nusers-file=test/invalid-users.csv\n"

logConfig :: Spec
logConfig =
  describe "Server configuration" $ do
    it "Should load complete server section" $ do
      parsed <- parse completeFile
      parsed `shouldBe` (Right $ LogConfiguration {lcConsoleLevel = DEBUG, lcFile = Just "/tmp/haskguard.log", lcFileLevel = ERROR})
    it "Should set default 'console level' to 'INFO'" $ do
      parsed <- parse ""
      lcConsoleLevel <$> parsed `shouldBe` Right INFO
    it "Should set default 'file' to nothing" $ do
      parsed <- parse ""
      lcFile <$> parsed `shouldBe` Right Nothing
    it "Should set default 'file level' to 'INFO'" $ do
      parsed <- parse ""
      lcFileLevel <$> parsed `shouldBe` Right INFO
    it "Should have an error when 'console level' is not a level" $ do
      parsed <- parse invalidConsoleLevel
      parsed `shouldBe` Left "Line 2, in section \"LOG\": \"NOPE\" is not a valid level (accepted are DEBUG, INFO and ERROR)"
    it "Should have an error when 'file level' is not a level" $ do
      parsed <- parse invalidFileLevel
      parsed `shouldBe` Left "Line 2, in section \"LOG\": \"FAKE\" is not a valid level (accepted are DEBUG, INFO and ERROR)"
  where
    parse fileContent = do
      parsed <- parseConfig fileContent
      case parsed of
        Right x -> pure $ Right $ gcLog x
        Left x -> pure $ Left x
    completeFile = "[LOG]\nconsole-level=DEBUG\nfile=/tmp/haskguard.log\nfile-level=ERROR\n"
    invalidConsoleLevel = "[LOG]\nconsole-level=NOPE\n"
    invalidFileLevel = "[LOG]\nfile-level=FAKE\n"
