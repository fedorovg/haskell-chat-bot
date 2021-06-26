module API.GenericAPISpec where

import API.GenericAPI
import qualified Config as C
import qualified Data.Map as M
import Test.Hspec


testApi1 = (GenericAPI "url.com" (QueryParam "tok" "123") (M.fromList [("kek", (Command "lol" 1))]) "help") 


spec = do
    describe "parseCommand" $ do
        it "should return help command in case of an error" $ do
            (parseCommand testApi1 "asfds") `shouldBe` (Command "help" 0, [])
            (parseCommand testApi1 "") `shouldBe` (Command "help" 0, [])
        it "should return command and params if names and param count match" $ do
            (parseCommand testApi1 "kek lol") `shouldBe` (Command "lol" 1,["lol"])

    describe "authToUrl" $ do
        it "Should correctly add params to the url" $ do
            authToUrl (QueryParam "a" "b") `shouldBe` "?a=b"
            authToUrl (Bearer "lel") `shouldBe` ""
            authToUrl (None) `shouldBe` ""

    describe "commandFromConf" $ do
        it "should transform CommandConf to key-Command pair" $ do
            commandFromConf (C.CommandConf "k" "n" 2) `shouldBe` ("k", (Command "n" 2))
    
    describe "makeUrl" $ do
        it "Should create a url from api with a command" $ do
            makeUrl testApi1 (Command "method" 2) ["kek", "lol"] `shouldBe` "url.com/method/kek/lol?tok=123"
            makeUrl testApi1 (Command "method" 0 ) [] `shouldBe` "url.com/method?tok=123"