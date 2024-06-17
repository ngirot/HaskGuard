module RequestSpec (spec) where

import Lib (Connection (..), request)
import Test.Hspec

spec :: Spec
spec = do
  connectIpv4
  connectIpv6
  connectDomainName

connectIpv4 :: Spec
connectIpv4 =
  describe "Connect ipv4" $ do
    it "Should accept ipv4" $ request [5, 1, 0, 1, 192, 168, 0, 1, 31, 144] `shouldBe` Right ([5, 0, 0, 1, 192, 168, 0, 1, 31, 144], Connection "192.168.0.1" "8080")
    it "Should reject payload with no command" $ request [5] `shouldBe` Left "Invalid payload size"
    it "Should reject payload with invalid ipv4 size" $ request [5, 1, 0, 1, 192, 168, 0, 31, 144] `shouldBe` Left "Invalid payload size"
    it "Should reject payload with invalid port size" $ request [5, 1, 0, 1, 192, 168, 0, 1, 31] `shouldBe` Left "Invalid payload size"

connectIpv6 :: Spec
connectIpv6 =
  describe "Connect ipv6" $ do
    it "Should accept ipv6" $ request [5, 1, 0, 4, 32, 1, 13, 184, 133, 163, 0, 0, 0, 0, 138, 46, 3, 112, 115, 52, 1, 187] `shouldBe` Right ([5, 0, 0, 4, 32, 1, 13, 184, 133, 163, 0, 0, 0, 0, 138, 46, 3, 112, 115, 52, 1, 187], Connection "2001:db8:85a3:0:0:8a2e:370:7334" "443")
    it "Should reject payload with no command" $ request [5] `shouldBe` Left "Invalid payload size"
    it "Should reject payload with invalid ipv6 size" $ request [5, 1, 0, 1, 32, 1, 13, 184, 133, 163, 0, 0, 0, 0, 138, 46, 3, 112, 115, 1, 187] `shouldBe` Left "Invalid payload size"
    it "Should reject payload with invalid por6 size" $ request [5, 1, 0, 1, 32, 1, 13, 184, 133, 163, 0, 0, 0, 0, 138, 46, 3, 112, 115, 52, 187] `shouldBe` Left "Invalid payload size"

connectDomainName :: Spec
connectDomainName =
  describe "Connect DNS" $ do
    it "Should accept DomainName" $ request [5, 1, 0, 3, 119, 119, 119, 46, 103, 111, 111, 103, 108, 101, 46, 99, 111, 109, 0, 80] `shouldBe` Right ([5, 0, 0, 3, 119, 119, 119, 46, 103, 111, 111, 103, 108, 101, 46, 99, 111, 109, 0, 80], Connection "www.google.com" "80")
    it "Should reject DomainName with no domain name" $ request [5, 1, 0, 3, 0, 80] `shouldBe` Left "Invalid payload size"