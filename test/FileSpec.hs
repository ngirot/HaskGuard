module FileSpec where

import File
import Test.Hspec

spec :: Spec
spec = do
  loadFile

loadFile :: Spec
loadFile =
  describe "Load file" $ do
    it "Should load file content" $ loadFileContent "test/test.file" `shouldReturn` Right "content of the file"
    it "Should load file content" $ loadFileContent "not-existing-file" `shouldReturn` Left FileDoesNotExists
