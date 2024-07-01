module ProtocolSpec where

import Payload
import Protocol
import Test.Hspec

spec :: Spec
spec = do
  negotiation
  findCommandSpec

negotiation :: Spec
negotiation =
  describe "Negotiation" $ do
    it "Should have no authentication protocol if available" $ findNegotiationReturnCode (NegotiationMessage 4 [0, 1, 2, 3]) `shouldBe` Right 0
    it "Should error code 255 when no authentication is not available" $ findNegotiationReturnCode (NegotiationMessage 4 [1, 2, 3]) `shouldBe` Left 255
    it "Should error code 255 when no methods are avaialbles" $ findNegotiationReturnCode (NegotiationMessage 4 []) `shouldBe` Left 255

findCommandSpec :: Spec
findCommandSpec =
  describe "Request command" $ do
    it "Should acccept CONNECT command" $ findCommand (RequestMessage 5 1 1 [] []) `shouldBe` Right 1
    it "Should reject BIND command" $ findCommand (RequestMessage 5 2 1 [] []) `shouldBe` Left 7
    it "Should reject UDP ASSOCIATE command" $ findCommand (RequestMessage 5 3 1 [] []) `shouldBe` Left 7
    it "Should reject not existing command" $ findCommand (RequestMessage 5 9 1 [] []) `shouldBe` Left 7
