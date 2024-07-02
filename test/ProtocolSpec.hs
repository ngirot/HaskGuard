module ProtocolSpec(spec) where

import Payload
import Protocol
import Test.Hspec

spec :: Spec
spec = do
  negotiation

negotiation :: Spec
negotiation =
  describe "Negotiation" $ do
    it "Should have no authentication protocol if available" $ findNegotiationReturnCode (NegotiationMessage 4 [0, 1, 2, 3]) `shouldBe` Right 0
    it "Should error code 255 when no authentication is not available" $ findNegotiationReturnCode (NegotiationMessage 4 [1, 2, 3]) `shouldBe` Left 255
    it "Should error code 255 when no methods are avaialbles" $ findNegotiationReturnCode (NegotiationMessage 4 []) `shouldBe` Left 255
