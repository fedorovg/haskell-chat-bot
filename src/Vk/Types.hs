{-# LANGUAGE TemplateHaskell #-}
{-| 
  This module contains types, used by Vk to comunicate with the Chat Bot
|-}
module Vk.Types where

import Data.Text (Text)
import Utils (deriveManyJSON, dropPrefixOptions)


newtype Sticker =
  Sticker
    { sStickerId :: Int
    } deriving (Show)

data Media =
  Media
    { mAccessKey :: Maybe Text
    , mId        :: Int
    , mOwnerId   :: Int
    } deriving (Show)

data Attachment =
  Attachment
    { aType    :: Text
    , aSticker :: Maybe Sticker
    , aPhoto   :: Maybe Media
    , aVideo   :: Maybe Media
    , aAudio   :: Maybe Media
    , aDoc     :: Maybe Media
    } deriving (Show)

data Message =
  Message
    { mFromId      :: Int
    , mText        :: Text
    , mRandomId    :: Int
    , mAttachments :: [Attachment]
    } deriving (Show)

newtype Object =
  Object
    { oMessage :: Maybe Message
    } deriving (Show)

data Update =
  Update
    { uObject :: Object
    , uType   :: Text
    } deriving (Show)

data ServerKeyTs =
  ServerKeyTs
    { sServer :: String
    , sKey    :: String
    , sTs     :: String
    } deriving (Eq, Show)

data Response =
  Response
    { rTs       :: Maybe String
    , rResponse :: Maybe ServerKeyTs
    , rUpdates  :: Maybe [Update]
    } deriving (Show)


$(deriveManyJSON dropPrefixOptions
    [ ''Sticker
    , ''Media
    , ''Attachment
    , ''Message
    , ''Object
    , ''Update
    , ''ServerKeyTs
    , ''Response
    ])
