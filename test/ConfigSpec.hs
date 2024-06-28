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
    it "Should load complete server section" $ parseIniFile completeFile configParser `shouldBe` (Right $ ServerConfiguration {scListen = "0.0.0.0", scPort = 80})
    it "Should set default 'port' value to '3128'" $ scPort <$> parseIniFile noPortFile configParser `shouldBe` Right 3128
    it "Should set default 'listen' value to 'localhost'" $ scListen <$> parseIniFile noListenFile configParser `shouldBe` Right "localhost"
    it "Should have default config on empty file" $ parseIniFile "" configParser `shouldBe` (Right $ ServerConfiguration {scListen = "localhost", scPort = 3128})
  where
    completeFile = "[SERVER]\nport=80\nlisten=0.0.0.0\n"
    noPortFile = "[SERVER]\nlisten=0.0.0.0\n"
    noListenFile = "[SERVER]\nport=80\n"
