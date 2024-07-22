module AuthenticationSpec (spec) where

import Authentication
import Config
import Errors
import Test.Hspec
import UserCsv

spec :: Spec
spec = do
  manageAuthenticationSpec

manageAuthenticationSpec :: Spec
manageAuthenticationSpec =
  describe "Authentication" $ do
    it "Should accept valid user" $ call [1, 4, 117, 115, 101, 114, 4, 112, 97, 115, 115] `shouldReturn` Right [1, 0]
    it "Should accept another valid user not first in the list" $ call [1, 2, 117, 50, 2, 112, 50] `shouldReturn` Right [1, 0]
    it "Should reject invalid user" $ call [1, 1, 114, 4, 112, 97, 115, 115] `shouldReturn` (Left $ ResponseError [1, 2])
    it "Should reject invalid password" $ call [1, 4, 117, 115, 101, 114, 1, 112] `shouldReturn` (Left $ ResponseError [1, 2])
  where
    configuration = AuthenticationConfiguration False True [Credentials "user" "pass", Credentials "u2" "p2"]
    call payload = manageAuthentication configuration (\_ -> return ()) payload
