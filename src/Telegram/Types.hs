{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

{-| 
  This module contains types, used by Telegram to comunicate with the Chat Bot
|-}
module Telegram.Types where

import Data.Text (Text)
import Utils (deriveManyJSON, dropPrefixOptions)

data Chat =
  Chat
    { cId        :: Int
    , cType      :: Text
    , cTitle     :: Maybe Text
    , cUsername  :: Maybe Text
    , cFirstName :: Maybe Text
    , cLastName  :: Maybe Text
    }  deriving (Show)

data Message =
  Message
    { mMessageId      :: Int
    , mFrom           :: User
    , mDate           :: Int
    , mChat           :: Chat
    , mForwardFrom    :: Maybe User
    , mReplyToMessage :: Maybe Message
    , mText           :: Maybe Text
    } deriving (Show)

data CallbackQuery =
  CallbackQuery
    { cData :: String
    , cFrom :: User
    }  deriving (Show)

data Update =
  Update
    { rUpdateId      :: Int
    , rMessage       :: Maybe Message
    , rCallbackQuery :: Maybe CallbackQuery
    , rData          :: Maybe Text
    , rEditedMessage :: Maybe Message
    } deriving (Show)

data Response =
  Response
    { rOk          :: Bool
    , rDescription :: Maybe String
    , rResult      :: Maybe [Update]
    , rErrorCode   :: Maybe Int
    }  deriving (Show)

data User =
  User
    { uId           :: Int
    , uIsBot        :: Bool
    , uFirstName    :: Text
    , uLastName     :: Maybe Text
    , uUsername     :: Maybe Text
    , uLanguageCode :: Maybe Text
    }  deriving (Show)

$(deriveManyJSON dropPrefixOptions
    [ ''Chat
    , ''Message
    , ''CallbackQuery
    , ''Update
    , ''Response
    , ''User
    ])
