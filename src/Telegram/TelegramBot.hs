{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Telegram.TelegramBot where

import Bot ( Bot(..) )
import Telegram.Types as TT
import GHC.Generics ( Generic )
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8





-- | Data type used as an instance of the Bot class to define interactions with the Telegram.
--  Contains only the Api key used to acces Telegram Api. 
data TelegramBot
    = TelegramBot
    { tbToken :: !String
    }

baseUrl = "https://api.telegram.org/bot"

-- | Data necessary to send a message through Telegram
data MessageDto
    = MessageDto
    { chat_id :: Int
    , text :: String
    } deriving (Generic, ToJSON, FromJSON)
 
-- | Bot instance for Telegarm
--   `type Destination TelegramBot = Int` In telegram user_id is used as destination for messages.
--   `type Session TelegramBot = Int` A simple Id of the last reveived message, every message with greater id is a new one.
instance Bot TelegramBot where
    type Destination TelegramBot = Int  
    type Session TelegramBot = Int 
    -- | Negative offset will result in telegram replying with the last message, that was sent to us.
    initialSession _ = -1 
    
    -- | getUpdates receives new messages form the Telegram and extracts sender and message from them.
    --   If session is equal to the Initial session, getUpdates makes request to get Id of the last message
    getUpdates bot manager session = undefined 

    -- | sendMesasge sends a POST request to the telegram to send a message as a response to a user.
    sendMessage bot manager dest message = undefined 
 
