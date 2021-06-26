{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Telegram.TelegramBot where

import Bot ( Bot(..) )
import Config
import Telegram.Types as TT
import GHC.Generics ( Generic )
import Data.Aeson
import Network.HTTP.Client
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Functor ((<&>))
import Control.Monad (liftM2, guard)
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Data.Traversable
import Data.Maybe (fromMaybe)
import qualified Data.Bifunctor


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
    getUpdates bot manager session = do
        s <- if session == initialSession bot then
            getSession bot manager
            else return session
        let req = parseRequest_ $ fullUrl bot <> "/getUpdates?offset=" <> show session <> "&timeout=25"
        response <-  httpLbs req manager -- simpleGet manager $ fullUrl bot <> "/getUpdates?offset=" <> show offset 
        let tgResponse = (decode (responseBody response) :: Maybe TT.Response)
        let mUpdates = newUpdates tgResponse
        let newSession = updateSession session tgResponse
        return $ case mUpdates of
            Just updates -> Right (newSession, map (Data.Bifunctor.second (L8.pack . T.unpack)) updates)
            _            -> if newSession == session
                                then Left "Failed to fetch updates"
                                else Right (newSession, []) 
    -- | sendMesasge sends a POST request to the telegram to send a message as a response to a user.
    sendMessage bot manager dest message = do
        initialRequest <- parseRequest $ fullUrl bot <> "/sendMessage"
        let request = initialRequest
                { method = "POST"
                , requestBody = RequestBodyLBS $ encode (MessageDto dest (L8.unpack message))
                , requestHeaders =
                     [ ("Content-Type", "application/json; charset=utf-8")
                     ]
                }
        response <- httpLbs request manager
        return $ responseBody response
     
getSession :: TelegramBot -> Manager -> IO (Session TelegramBot)
-- ^ getSession makes the same request as the getUpdates funtion, but with a negative offset, so we can learn what is the latest message, that was sent to us
--   so we can read messages starting from the next one. (Haddock couldn't parse commets above this function for some reason)
getSession b m = do
    let req = parseRequest_ $ fullUrl b <> "/getUpdates?offset=-1&timeout=25"
    response <-  httpLbs req m
    let tgResponse = (decode (responseBody response) :: Maybe TT.Response)
    return $ updateSession (initialSession b) tgResponse

-- | getTextAndChatId simply extracts two necessery fields from the Telegram update, if they are present
getTextAndChatId :: TT.Update -> Maybe (Int, T.Text)
getTextAndChatId u = liftM2 (,) chatId messageText
    where message = rMessage u <|> rEditedMessage u
          chatId = message <&> mChat <&> cId
          messageText = message >>= mText

-- | newUpdates applies getTextAndChatId to all updates in a Response
newUpdates ::  Maybe TT.Response -> Maybe [(Int, T.Text)]
newUpdates r = (r >>= rResult) >>= traverse getTextAndChatId 

-- | updateSession goes through the updates and finds the latest one, so we can read updates from there on. In case of an error session stays the same.
updateSession :: Session TelegramBot -> Maybe TT.Response -> Session TelegramBot
updateSession oldSession r =
    fromMaybe oldSession $ (nonEmptyOrNothing mUpdates <&> map rUpdateId <&> maximum) <&> (+ 1)
    where mUpdates = r >>= rResult
          nonEmptyOrNothing Nothing = Nothing
          nonEmptyOrNothing (Just []) = Nothing
          nonEmptyOrNothing x = x

fullUrl :: TelegramBot -> String
fullUrl bot = baseUrl <> tbToken bot

createBot :: Config -> TelegramBot
createBot (Config (TelegramConfig token) _) = TelegramBot token
createBot _ = error "Invalid config type."
