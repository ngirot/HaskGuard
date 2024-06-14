module RequestSpec (spec) where

import Lib (Connection(..), request)
import Test.Hspec

spec :: Spec
spec = do
  connect

connect :: Spec
connect =
  describe "Connect" $ do
    it "Should accept ipv4" $  request [5, 1, 0, 1, 192, 168, 0, 1, 31, 144] `shouldBe` ([5, 0, 0, 1, 192, 168, 0, 1, 31, 144], Connection "192.168.0.1" "8080")