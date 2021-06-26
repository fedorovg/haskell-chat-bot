module UtilsSpec (spec) where

import Utils
import Test.Hspec

spec = do
    describe "dropPrefix" $ do
        it "should remove first prefix of lowercase chars" $ do
            dropPrefix "lolKek" `shouldBe` "kek"
            dropPrefix "lolFooBar" `shouldBe` "fooBar"
            dropPrefix "Lol" `shouldBe` "lol"

