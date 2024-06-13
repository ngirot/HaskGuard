module FakeSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  fake

fake :: Spec
fake =
  describe "Fake test" $ do
    it "Should be True" $ True `shouldBe` True