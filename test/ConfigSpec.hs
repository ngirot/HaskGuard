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

serverConfig :: Spec
serverConfig =
  describe "Server configuration" $ do
    it "Should load complete server section" $ parse completeFile `shouldBe` (Right $ ServerConfiguration {scListen = "localhost", scPort = 80})
    it "Should set default 'port' value to '3128'" $ scPort <$> parse "" `shouldBe` Right 3128
    it "Should set default 'listen' value to listen everything" $ scListen <$> parse "" `shouldBe` Right "0.0.0.0"
    it "Should have an error when 'port' is not a valid number" $ parse invalidPort `shouldBe` Left "Line 2, in section \"SERVER\": Unable to parse \"test\" as a value of type Int"
  where
    parse fileContent = acServer <$> parseIniFile fileContent configParser
    completeFile = "[SERVER]\nport=80\nlisten=localhost\n"
    invalidPort = "[SERVER]\nport=test\n"

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
    parse fileContent = acLog <$> parseIniFile fileContent configParser
    completeFile = "[LOG]\nconsole-level=DEBUG\nfile=/tmp/haskguard.log\nfile-level=ERROR\n"
    invalidConsoleLevel = "[LOG]\nconsole-level=NOPE\n"
    invalidFileLevel = "[LOG]\nfile-level=FAKE\n"
