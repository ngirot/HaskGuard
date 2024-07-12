{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Config
import Data.Ini.Config
import Test.Hspec

spec :: Spec
spec = do
  serverConfig

serverConfig :: Spec
serverConfig =
  describe "Server configuration" $ do
    it "Should load complete server section" $ parseIniFile completeFile configParser `shouldBe` (Right $ ServerConfiguration {scListen = "localhost", scPort = 80})
    it "Should set default 'port' value to '3128'" $ scPort <$> parseIniFile noPortFile configParser `shouldBe` Right 3128
    it "Should set default 'listen' value to listen everything" $ scListen <$> parseIniFile noListenFile configParser `shouldBe` Right "0.0.0.0"
    it "Should have default config on empty file" $ parseIniFile "" configParser `shouldBe` (Right $ ServerConfiguration {scListen = "0.0.0.0", scPort = 3128})
    it "Should have an error when port is not a valid number" $ scPort <$> parseIniFile invalidPort configParser `shouldBe` Left "Line 2, in section \"SERVER\": Unable to parse \"test\" as a value of type Int"
  where
    completeFile = "[SERVER]\nport=80\nlisten=localhost\n"
    noPortFile = "[SERVER]\nlisten=0.0.0.0\n"
    noListenFile = "[SERVER]\nport=80\n"
    invalidPort = "[SERVER]\nport=test\n"
