module Vk.VkBotSpec (spec) where

import Vk.VkBot
import Vk.Types
import Test.Hspec


skt = ServerKeyTs "server" "key" "1"

stickerUpdate = Update
    { uObject =
      Object
        { oMessage =
           Just Message
               { mFromId = 12345678
               , mText = ""
               , mRandomId = 0
               , mAttachments =
                    [Attachment
                      { aType = "sticker"
                      , aSticker = Just $ Sticker 9046
                      , aPhoto = Nothing
                      , aVideo = Nothing
                      , aAudio = Nothing
                      , aDoc = Nothing
                      }
                    ]
               }
        }
  , uType = "message_new"
  }

update
    = Update
    { uObject =
        Object
          { oMessage =
             Just Message
                 { mFromId = 12345
                 , mText = "text1"
                 , mRandomId = 0
                 , mAttachments = []
                 }
          }
          , uType = "message_new"
    }

update1
    = Update
    { uObject =
        Object
          { oMessage =
             Just Message
                 { mFromId = 12345678
                 , mText = "text12"
                 , mRandomId = 0
                 , mAttachments = []
                 }
          }
          , uType = "message_new"
    }

response 
    = Just $ Response
    { rTs = Just "12"
    , rResponse = Just (ServerKeyTs "server" "key" "2")
    , rUpdates = Just ([update, update1, stickerUpdate])
    }

spec = do
    describe "extractTextAndSender" $ do
        it "should extract data from update with new message" $ do
            extractTextAndSender update `shouldBe` Just (12345,"text1")
            extractTextAndSender update1 `shouldBe` Just (12345678,"text12")

        it "should return empty message in case of a message with an attachment" $ do
            extractTextAndSender stickerUpdate `shouldBe` Just (12345678,"")
        
    describe "newUpdates" $ do
        it "should extract data from update with new message" $ do
            newUpdates response `shouldBe` (Just [(12345,"text1"),(12345678,"text12"),(12345678,"")])

    describe "nextSession" $ do
        it "Should replace the session with the one extracted from Response if present" $ do
            nextSession skt response `shouldBe` ServerKeyTs {sServer = "server", sKey = "key", sTs = "12"}
            nextSession skt Nothing `shouldBe` skt
        