module ConfigSpec where

import Test.Hspec
import Config
import Data.Aeson

spec = do
    describe "BotConfig should be an instance of fromJSON" $ do
        it "Should parse Telegram config" $ do
            (decode "{\"platform\": \"telegram\",\"telegram\": {\"token\": \"12345\"},\"vk\": {\"key\": \"kkek\",\"clubId\": \"1234567\"}}" :: Maybe BotConfig) `shouldBe` (Just (TelegramConfig "12345"))
        it "Should parase Vk config" $ do
            (decode "{\"platform\": \"vk\",\"telegram\": {\"token\": \"12345\"},\"vk\": {\"key\": \"kkek\",\"clubId\": \"1234567\"}}" :: Maybe BotConfig) `shouldBe` (Just (VkConfig "kkek" "1234567"))

    describe "ApiConfig should be an instance of fromJSOM" $ do
        it "Should parse Weather config" $ do
            (decode "{\"type\": \"weather\",\"help\": \"h\",\"weather\": {\"url\": \"www\",\"token\": \"tok\"},\"generic\": {\"url\": \"www\",\"auth\": {\"tag\": \"Bearer\",\"token\": \"tok\"},\"commands\": []}}" :: Maybe ApiConfig) `shouldBe` (Just (WeatherApiConfig "www" "tok" "h"))
        it "Should parse Generic config" $ do
            (decode "{\"type\": \"generic\",\"help\": \"h\",\"weather\": {\"url\": \"www\",\"token\": \"tok\"},\"generic\": {\"url\": \"www\",\"auth\": {\"tag\": \"Bearer\",\"token\": \"tok\"},\"commands\": []}}" :: Maybe ApiConfig) `shouldBe` (Just (GenericApiConfig "www" (Bearer "tok") [] "h"))
            (decode "{\"type\": \"generic\",\"help\": \"h\",\"weather\": {\"url\": \"www\",\"token\": \"tok\"},\"generic\": {\"url\": \"www\",\"auth\": {\"tag\": \"Bearer\",\"token\": \"tok\"},\"commands\": [{\"key\": \"com\",\"name\": \"test\",\"params\": 1}]}}" :: Maybe ApiConfig) `shouldBe` (Just (GenericApiConfig "www" (Bearer "tok") [CommandConf {ccKey = "com", ccName = "test", ccParams = 1}] "h"))
    
    