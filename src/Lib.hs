-- {-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib (start) where


import Bot
import Prelude hiding (log)
import Control.Monad.Reader
import Data.Text ( unpack, Text )
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Concurrent (threadDelay)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Config
import qualified Logging as Log
import Telegram.TelegramBot as TG
import API.Weather
import API.DataAPI
import API.GenericAPI
import qualified Data.Map as M
import Vk.VkBot

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

-- | Main loop of the application. 
--   On each run this function checks for updates, fetches responses for these updates and sends a response for eah one.
run :: (Bot b, MonadReader (Env b) m, MonadIO m) => Session b ->  m ()
run session = do
    env <- ask
    let eGetUpdates = envGetUpdates env
    let eSendMessage = envSendMessage env
    let eGetData = envGetData env
    let eInitialSession = envInitialSession env
    let elogLevel = envLog env

    liftIO $ Log.logInfo elogLevel "Fetching updates..."
    eitherUpdates <- liftIO $ eGetUpdates session
    liftIO $ Log.logDebug elogLevel ""
    case eitherUpdates of
        Right (newSession, newMessages) -> do
            liftIO $ Log.logInfo elogLevel $  "Received " <> (show . length $ newMessages) <> " new messages."
            rs <- liftIO $ traverse (eGetData . L8.unpack . snd) newMessages
            liftIO $ Log.logDebug elogLevel $ show rs
            liftIO $ traverse (uncurry eSendMessage) (zip (map fst newMessages) rs)
            runReaderT (run newSession) env
        Left errMsg -> do
            liftIO . Log.logError $ L8.unpack errMsg
            liftIO . Log.logInfo elogLevel $ "Waiting for 10 seconds before another poll."
            liftIO . threadDelay $ 1000000 * 10
            runReaderT (run eInitialSession) env

-- | The function, that creates Bot and Env from a config and runs the Application.
start :: FilePath -> IO ()
start configPath = do
    eitherConfig <- readConfig configPath
    manager <- newManager tlsManagerSettings
    case eitherConfig of
        Left errorMessage -> Log.logError errorMessage
        Right config@(Config (TelegramConfig _) _) -> do
            let tgBot = TG.createBot config
            let env = createEnv tgBot config manager
            runReaderT (run (initialSession tgBot)) env
        Right config@(Config (VkConfig _ _) _) -> do
            let vBot = Vk.VkBot.createBot config
            let env = createEnv vBot config manager
            runReaderT (run (initialSession vBot)) env
        