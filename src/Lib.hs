module Lib
    ( start
    ) where

import qualified Logging as Log
import Bot
import qualified Data.ByteString.Lazy.Char8 as L8
import Config
import Network.HTTP.Client
import API.Weather
import API.DataAPI
import API.GenericAPI

-- | Env contains the read-only application environment.
--   It stores all functions and data used by the application in runtime.
data Env b
    = Env
    { envLog :: !Log.Level -- ^ A log level, so that we can easily disable unnecesasry logging
    , envGetUpdates :: !(Session b -> IO (Either L8.ByteString (Session b, [(Destination b, L8.ByteString)]))) -- ^ Long polling funciton used to check for new messages
    , envSendMessage :: Destination b -> L8.ByteString  -> IO L8.ByteString -- ^ Function to respond with a message
    , envGetData :: !(String -> IO L8.ByteString)  -- ^ Get data, used as a response for the message 
    , envInitialSession :: Session b -- ^ Start state for our bot
    }


-- | createEnv is afunction that creates application `Env`ironment from provided Bot implementation and a Config
createEnv :: Bot b => b -> Config -> Manager -> Env b
createEnv bot config@(Config _ apiConf) manager =
    case apiConf of
        (WeatherApiConfig url tok help) ->
            Env
                Log.Info
                (getUpdates bot manager)
                (sendMessage bot manager)
                (getData api manager)
                (initialSession bot)
                where api = WeatherAPI tok url help
        aCfg@GenericApiConfig {} ->
             Env
                Log.Info
                (getUpdates bot manager)
                (sendMessage bot manager)
                (getData api manager)
                (initialSession bot)
                where api = createGenericApi aCfg

start :: FilePath -> IO ()
start configPath = undefined 