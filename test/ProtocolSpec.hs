module ProtocolSpec (spec) where

import Payload
import Protocol
import Test.Hspec

spec :: Spec
spec = do
  findNegotiationReturnCodeSpec
  findPortSpec
  findCommandSpec
  findIpSpec

findNegotiationReturnCodeSpec :: Spec
findNegotiationReturnCodeSpec =
  describe "findNegotiationReturnCode" $ do
    it "Should have no authentication protocol if available" $ findNegotiationReturnCode (NegotiationMessage 4 [0, 1, 2, 3]) `shouldBe` Right 0
    it "Should error code 255 when no authentication is not available" $ findNegotiationReturnCode (NegotiationMessage 4 [1, 2, 3]) `shouldBe` Left 255
    it "Should error code 255 when no methods are avaialbles" $ findNegotiationReturnCode (NegotiationMessage 4 []) `shouldBe` Left 255

findPortSpec :: Spec
findPortSpec =
  describe "findPort" $ do
    it "Should find port on single byte" $ findPort (RequestMessage 1 1 1 [] [0, 80]) `shouldBe` "80"
    it "Should find port on all bytes" $ findPort (RequestMessage 1 1 1 [] [1, 187]) `shouldBe` "443"

findCommandSpec :: Spec
findCommandSpec =
  describe "findCommand" $ do
    it "Should find CONNECT command" $ findCommand (RequestMessage 1 1 1 [] []) `shouldBe` Right Connect
    it "Should send error code 7 on BIND command" $ findCommand (RequestMessage 1 2 1 [] []) `shouldBe` Left 7
    it "Should send error code 7 on UDP ASSOCIATE command" $ findCommand (RequestMessage 1 3 1 [] []) `shouldBe` Left 7
    it "Should send error code 7 on invalid command number" $ findCommand (RequestMessage 1 9 1 [] []) `shouldBe` Left 7

findIpSpec :: Spec
findIpSpec =
  describe "findPort" $ do
    it "Should find ip v4 on request" $ findIp (RequestMessage 1 1 1 [192, 168, 10, 35] []) `shouldBe` Right "192.168.10.35"
    it "Should find ip v6 on request" $ findIp (RequestMessage 1 1 4 [32, 1, 13, 184, 133, 163, 0, 0, 0, 0, 138, 46, 3, 112, 115, 52] []) `shouldBe` Right "2001:db8:85a3:0:0:8a2e:370:7334"
    it "Should find domain name on request" $ findIp (RequestMessage 1 1 3 [119, 119, 119, 46, 103, 111, 111, 103, 108, 101, 46, 99, 111, 109] []) `shouldBe` Right "www.google.com"
    it "Should return error when type in not valid" $ findIp (RequestMessage 1 1 2 [1] []) `shouldBe` Left 8
