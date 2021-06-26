module Telegram.TelegramBotSpec (spec) where

import Test.Hspec
import Telegram.Types as TT
import Telegram.TelegramBot
import Data.Text

formUpdate :: Text -> Update
formUpdate t =
    Update
      { rUpdateId = 783724748
      , rMessage =
         Just (Message
           { mMessageId = 100
           , mFrom =
              User
                { uId = 123456789
                , uIsBot = False
                , uFirstName = "UserName"
                , uLastName = Nothing
                , uUsername = Just "Nickname"
                , uLanguageCode = Just "ru"
                }
            , mDate = 1000000000
            , mChat =
                Chat
                  { cId = 123456789
                  , cType = "private"
                  , cTitle = Nothing
                  , cUsername = Just "Nickname"
                  , cFirstName = Just "UserName"
                  , cLastName = Nothing
                  }
             , mForwardFrom = Nothing
             , mReplyToMessage = Nothing
             , mText = Just t
             })
      , rCallbackQuery = Nothing
      , rData = Nothing
      , rEditedMessage = Nothing
      }

formUpdateEdited :: Text -> Update
formUpdateEdited t =
    Update
      { rUpdateId = 783724748
      , rMessage = Nothing
      , rCallbackQuery = Nothing
      , rData = Nothing
      , rEditedMessage = Just (Message
           { mMessageId = 100
           , mFrom =
              User
                { uId = 123456789
                , uIsBot = False
                , uFirstName = "UserName"
                , uLastName = Nothing
                , uUsername = Just "Nickname"
                , uLanguageCode = Just "ru"
                }
            , mDate = 1000000000
            , mChat =
                Chat
                  { cId = 123456789
                  , cType = "private"
                  , cTitle = Nothing
                  , cUsername = Just "Nickname"
                  , cFirstName = Just "UserName"
                  , cLastName = Nothing
                  }
             , mForwardFrom = Nothing
             , mReplyToMessage = Nothing
             , mText = Just t
             })
      }

response = Just $ Response
    { rOk = True
    , rDescription = Nothing
    , rResult = Just (Prelude.map formUpdate ["lol", "kek", "no"])
    , rErrorCode = Nothing 
    }

spec = do
    describe "getTextAndChatId" $ do
        it "should extract data from update with new message" $ do
            getTextAndChatId (formUpdate "lol") `shouldBe` (Just (123456789,"lol"))
        
        it "Should extract data from update with edited message" $ do
            getTextAndChatId (formUpdateEdited "kek") `shouldBe` (Just (123456789,"kek"))

    describe "newUpdates" $ do
        it "Should extract pairs of data from the whole response" $ do
            newUpdates response `shouldBe` Just [(123456789,"lol"),(123456789,"kek"),(123456789,"no")]
            newUpdates Nothing `shouldBe` Nothing

    describe "updateSession" $ do
        it "Should find update with the latest offset and update session if present" $ do
            updateSession 1 response `shouldBe` 783724749
            updateSession 1 Nothing `shouldBe` 1