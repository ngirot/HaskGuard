module UserCsvSpec (spec) where

import Test.Hspec
import UserCsv

spec :: Spec
spec = do
  loadUserCsvSpec

loadUserCsvSpec :: Spec
loadUserCsvSpec =
  describe "Load user CSV" $ do
    it "Should load empty CSV" $ parseCsv "" `shouldBe` Right []
    it "Should parse valid one line CSV" $ parseCsv "\"user\",\"password\"" `shouldBe` Right [Credentials "user" "password"]
    it "Should reject line with only one element" $ parseCsv "user" `shouldBe` Left "Unable to parse CSV: parse error (Failed reading: conversion error: cannot unpack array of length 1 into a pair. Input record: [\"user\"]) at \"\""
    it "Should reject line with more than two elements" $ parseCsv "user,password,extra" `shouldBe` Left "Unable to parse CSV: parse error (Failed reading: conversion error: cannot unpack array of length 3 into a pair. Input record: [\"user\",\"password\",\"extra\"]) at \"\""
