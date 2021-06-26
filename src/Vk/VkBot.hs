{-# LANGUAGE TypeFamilies #-}
module Vk.VkBot where

import Vk.Types as VT
import Bot
import qualified Config as C
import Network.HTTP.Client
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe
import qualified Data.Text as T
import Data.Functor ((<&>))
import Control.Monad (liftM2)
import qualified Network.URI.Encode as URI
import System.Random

-- | VkBot is a data type used as an instance of the Bot class to define interactions with the Vk.
--  Contains baseUrl, token used for authentication and an id of a Vk group, the bot is attached to. 
data VkBot
    = VkBot
    { baseUrl :: String
    , token :: String
    , clubId :: String
    }
-- | VkMessage is DTO used to send messages via Vk
data VkMessage
    = VkMessage
    { user_id :: Int -- ^ Id of the receoient
    , random_id :: Int -- ^ A quirk of the vk API. It requires, that all messages have a ranom number attached to them
    , message :: String -- ^ the message itself
    }

-- | Instance of Bot for Telegram
--   Unlike telegarm, Vk uses two Apis for chat bots. 
--   The main one is used to get accses to the LongPolling server and SendMessages.
--   LongPolling api is accessed via separate url and token, returned from the Main API.
--   All upadtes are fetched from the LongPollingAPI
--   `type Destination VkBot = Int` Destination is just an Id of a message recepient
--   `type Session VkBot = ServerKeyTs` Session contains a url of LongPolling server, temporary token and id of the latest message. All of this is provided by Vk after fetching the Long Poll server from the main API.
instance Bot VkBot where
    type Destination VkBot = Int 
    type Session VkBot = ServerKeyTs
    -- | initialSession when we do not have the long polling server yet
    initialSession _ = ServerKeyTs  "" "" "0" 
    -- | getUpdates aquires the long polling server info, if it is not yet present and then uses it get Updates about the new messages from the Vk.
    getUpdates bot manager session@(ServerKeyTs server key ts) = do
        mSession <- if session == initialSession bot
            then getSession bot manager
            else return $ Just session

        case mSession of
            Just s -> do
                mResponse <- fetchUpdates manager s
                let nSession = nextSession s mResponse
                let mTextSenderPairs = newUpdates mResponse
                let textSenderPairs = fromMaybe [] mTextSenderPairs
                return $ Right (nSession, textSenderPairs)
            Nothing -> return $ Left "kek"

    -- | sendMessage simply sends a message through Vk using VkMessage DTO to a provided destination.
    sendMessage bot manager dest message = do
        initialRequest <- parseRequest $ baseUrl bot <> "/messages.send"
        randId <- randomRIO (1000000,  maxBound :: Int) :: IO Int
        let reqBody = sendMessageURI (VkMessage dest randId (L8.unpack message)) <> "&" <> defaultUrlParams bot
        let request = initialRequest
                { method = "POST"
                , requestBody = RequestBodyLBS $ 
                    L8.pack reqBody
                , requestHeaders =
                     [ ("Content-Type", "application/x-www-form-urlencoded")
                     ]
                }
        response <- httpLbs request manager
        return $ responseBody response
        

sendMessageURI :: VkMessage -> String
sendMessageURI (VkMessage uId rId m) = "user_id=" <> show uId  <> "&random_id="<> show rId <>"&message=" <> URI.encode m

-- | Gets the id of the latest message (if present) and updates the session
nextSession :: ServerKeyTs -> Maybe VT.Response -> ServerKeyTs
nextSession (ServerKeyTs s k t) o = ServerKeyTs s k (fromMaybe t newTs)
    where newTs = o >>= rTs

toUrl :: ServerKeyTs -> String
toUrl skt = sServer skt <> "?act=a_check&key=" <> sKey skt <> "&ts=" <> sTs skt <> "&wait=25"

-- | extractTextAndSender gets user_id and message text from an update if present
extractTextAndSender :: VT.Update -> Maybe (Int, L8.ByteString)
extractTextAndSender u = liftM2 (,) s t
    where t = (oMessage (uObject u) <&> mText) <&> L8.pack . T.unpack
          s = oMessage (uObject u) <&> mFromId

-- | newUpdates appliese extractTextAndSender to all updates in a Response
newUpdates :: Maybe VT.Response ->  Maybe [(Int, L8.ByteString)]
newUpdates r = r >>= rUpdates >>= traverse  extractTextAndSender

-- | Function that actually makes a request to LongPolling API to get updates
fetchUpdates :: Manager -> Session VkBot -> IO (Maybe VT.Response)
fetchUpdates manager s = do
    let req = parseRequest_ (toUrl s)
    response <- httpLbs req manager
    let body = responseBody response
    return (decode body :: Maybe VT.Response)

-- | Function that makes request to the Main API to get access to the LongPolling API
getSession :: VkBot -> Manager-> IO (Maybe (Session VkBot))
getSession bot manager= do
    let req = parseRequest_ (baseUrl bot <> "/groups.getLongPollServer?" <> defaultUrlParams bot)
    response <- httpLbs req manager
    return $
        (decode (responseBody response) :: Maybe VT.Response)
        >>= rResponse

defaultUrlParams :: VkBot -> String
defaultUrlParams bot =  "v=5.131&group_id=" <> clubId bot <> "&access_token=" <> token bot

createBot :: C.Config -> VkBot
createBot (C.Config (C.VkConfig key clubId) _ ) = VkBot "https://api.vk.com/method" key clubId