module PayloadSpec where

import Payload
import Test.Hspec

spec :: Spec
spec = do
  negotiationInput
  negotiationOutput
  requestInput
  requestOutput

negotiationInput :: Spec
negotiationInput =
  describe "Negotiation input" $ do
    it "Should parse negotiation input with one method" $ parseNegotiationInput [5, 1, 0] `shouldBe` (Right $ NegotiationMessage 5 [0])
    it "Should refuse payload with only version" $ parseNegotiationInput [5] `shouldBe` Left "Payload has invalid size"
    it "Should refuse payload less authentication method than declared" $ parseNegotiationInput [5, 3, 0] `shouldBe` Left "Payload has invalid size"
    it "Should refuse payload more authentication method than declared" $ parseNegotiationInput [5, 1, 0, 1, 2] `shouldBe` Left "Payload has invalid size"

negotiationOutput :: Spec
negotiationOutput =
  describe "Negotiation output" $ do
    it "Should create output" $ (generateNegotiationOutput (NegotiationMessage 5 []) 7) `shouldBe` [5, 7]

requestInput :: Spec
requestInput =
  describe "Request input" $ do
    it "Should reject payload with only version" $ parseRequestInput [5] `shouldBe` Left "Invalid payload size"
    it "Should accept ipv4" $ parseRequestInput [5, 1, 0, 1, 192, 168, 0, 1, 31, 144] `shouldBe` (Right $ RequestMessage 5 1 1 [192, 168, 0, 1] [31, 144])
    it "Should reject payload with invalid ipv4 size" $ parseRequestInput [5, 1, 0, 1, 192, 168, 0, 31, 144] `shouldBe` Left "Invalid payload size"
    it "Should reject payload with invalid port size" $ parseRequestInput [5, 1, 0, 1, 192, 168, 0, 1, 31] `shouldBe` Left "Invalid payload size"
    it "Should accept ipv6" $ parseRequestInput [5, 1, 0, 4, 32, 1, 13, 184, 133, 163, 0, 0, 0, 0, 138, 46, 3, 112, 115, 52, 1, 187] `shouldBe` (Right $ RequestMessage 5 1 4 [32, 1, 13, 184, 133, 163, 0, 0, 0, 0, 138, 46, 3, 112, 115, 52] [1, 187])
    it "Should reject payload with invalid ipv6 size" $ parseRequestInput [5, 1, 0, 1, 32, 1, 13, 184, 133, 163, 0, 0, 0, 0, 138, 46, 3, 112, 115, 1, 187] `shouldBe` Left "Invalid payload size"
    it "Should reject payload with invalid port size" $ parseRequestInput [5, 1, 0, 1, 32, 1, 13, 184, 133, 163, 0, 0, 0, 0, 138, 46, 3, 112, 115, 52, 187] `shouldBe` Left "Invalid payload size"
    it "Should accept DomainName" $ parseRequestInput [5, 1, 0, 3, 119, 119, 119, 46, 103, 111, 111, 103, 108, 101, 46, 99, 111, 109, 0, 80] `shouldBe` (Right $ RequestMessage 5 1 3 [119, 119, 119, 46, 103, 111, 111, 103, 108, 101, 46, 99, 111, 109] [0, 80])
    it "Should reject DomainName with no domain name" $ parseRequestInput [5, 1, 0, 3, 0, 80] `shouldBe` Left "Invalid payload size"

requestOutput :: Spec
requestOutput =
  describe "Request output" $ do
    it "Should create output" $ (generateRequestOutput (RequestMessage 5 1 1 [192, 168, 0, 1] [0, 80]) 7) `shouldBe` [5, 7, 0, 1, 192, 168, 0, 1, 0, 80]
