module NegotiationSpec (spec) where

import Lib
import Test.Hspec

spec :: Spec
spec = do
  socks5

socks5 :: Spec
socks5 =
  describe "Socks5" $ do
    it "Should accept no authentication" $ negotiate [5, 1, 0] `shouldBe` [5, 0]
    it "Should select no authentication when avaialbe" $ negotiate [5, 3, 0, 1, 2] `shouldBe` [5, 0]
    it "Should refuse when authentication is required" $ negotiate [5, 2, 1, 2] `shouldBe` [5, 255]
    it "Should refuse payload with only version" $ negotiate [5] `shouldBe` [5, 255]
    it "Should refuse payload less authentication method than declared" $ negotiate [5, 3, 0] `shouldBe` [5, 255]
    it "Should refuse payload more authentication method than declared" $ negotiate [5, 1, 0, 1, 2] `shouldBe` [5, 255]
