module FileSpec (spec) where

import File
import Test.Hspec

spec :: Spec
spec = do
  loadFile

loadFile :: Spec
loadFile =
  describe "Load file" $ do
    it "Should load file content" $ loadFileContent "test/test.file" `shouldReturn` Right "content of the file"
    it "Should have an error when file does not exists" $ loadFileContent "not-existing-file" `shouldReturn` Left FileDoesNotExists
