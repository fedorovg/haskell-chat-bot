{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Bot where

import Network.HTTP.Client
import qualified Data.ByteString.Lazy.Char8 as L8

-- | The type class used to generalize our Chat bot.
--   Bot contains signatures, that are necessary for every bot implementation.
class Eq (Session b) =>  Bot b where
    -- | A type used to definde a message recepient. Mostly a user id.
    type Destination b 
    -- | Session a type that contains information between requests, used to distinguish old messages from the new ones.
    type Session b 
    -- | Long poll function to check for any new messages.
    getUpdates :: b -> Manager -> Session b -> IO (Either L8.ByteString (Session b, [(Destination b, L8.ByteString)])) 
    -- | Responds with a message to a Provided destination
    sendMessage :: b -> Manager -> Destination b -> L8.ByteString  -> IO L8.ByteString 
    -- | Initial state of the Bot, when it has not polled for updates
    initialSession :: b -> Session b 


            