{-# LANGUAGE TemplateHaskell #-}

module Config where


import qualified Data.ByteString.Char8 as B 
import Data.Either ()
import Data.Text ( Text )
import Control.Exception ( try )
import System.IO.Error (illegalOperationErrorType)
import Data.Aeson
import Utils

data Platform = Telegram | Vkontakte deriving (Show)

instance FromJSON Platform where
    parseJSON = withText "platform" $ \case
        "telegram" -> return Telegram
        "vk" -> return Vkontakte
        _ -> fail "string is not one of known enum values [PLatfrom]"

data BotType = Weather | Generic deriving (Show)

instance FromJSON BotType where
    parseJSON = withText "type" $ \case
        "generic" -> return Generic
        "weather" -> return Weather
        _ -> fail "string is not one of known enum values"

-- | A data type, that contains the whole Configuration of the application.
data Config 
    = Config
    { cBotConfig :: BotConfig
    , cApiConfig :: ApiConfig
    } deriving (Show, Eq)

-- | Contains necessary data for running bot on a specified platfrorm.
data BotConfig
    =  TelegramConfig -- ^ Contains only a token.
    { bcToken :: String
    } 
    | VkConfig 
    { bcKey :: String
    , bcClubId :: String -- ^ ClubId is the id of a Vk group this bot is attached to
    } deriving (Show, Eq)

-- | Contains configurtations for the API, where response data is fetched from.
data ApiConfig
    = WeatherApiConfig -- ^ A config for predfined API for working with open weather API
    { acUrl :: String
    , acToken :: String
    , acHelp :: String
    }
    | GenericApiConfig -- ^ A config for a generic API, defined by this Config
    { acUrl :: String -- ^ Base url address of the API.
    , acAuth :: ApiAuthConf -- ^ Authentication method used to access this API.
    , acCommands :: [CommandConf] -- ^ Commands exposed by the API.
    , acHelp :: String
    } deriving (Show, Eq)

-- | Contains configureation for Authentication method used to access a Generic Api
data ApiAuthConf
    = Bearer -- ^ Passes Berare <Token> header to all requests to the API
    { aacToken :: String
    }
    | QueryParam -- ^ Adds an aditional url param to all requests to the API
    { acName :: String
    , aacToken :: String
    }
    | None deriving (Show, Eq)

-- | Defines commands provided by the data api.
data CommandConf
    = CommandConf
    { ccKey :: String -- ^ key of the command. The first word in users's message
    , ccName:: String -- ^ name of the command. The command added to the api url to make a request ( <apiUrl>/<ccName> )
    , ccParams :: Int -- ^ Number of parameters, that are expected by the command.
    } deriving (Show, Eq)


instance FromJSON BotConfig where
    parseJSON = withObject "bot" $ \oBot -> do
        platform <- oBot .: "platform"
        case platform of 
            Telegram -> do
                oTg <- oBot .: "telegram"
                bcToken <- oTg .: "token"
                return TelegramConfig {..}
            Vkontakte -> do
                oVk <- oBot .: "vk"
                bcKey <- oVk .: "key"
                bcClubId <- oVk .: "clubId"
                return VkConfig {..}

instance FromJSON ApiConfig where
    parseJSON = withObject "api" $ \oApi -> do
        botType <- oApi .: "type"
        acHelp <- oApi .: "help"
        case botType of
            Weather -> do
                oW <- oApi .: "weather"
                acUrl <- oW .: "url"
                acToken <- oW .: "token"
                return WeatherApiConfig {..}
            Generic -> do
                oG <- oApi .: "generic"
                acUrl <- oG .: "url"
                acAuth <- oG .: "auth"
                acCommands <- oG .: "commands"
                return GenericApiConfig {..}
       
instance FromJSON Config where
    parseJSON = withObject "config" $ \o -> do
        cBotConfig <- o .: "bot"
        cApiConfig <- o .: "api"
        return Config {..}

$(deriveManyJSON dropPrefixOptions
    [ ''CommandConf
    , ''ApiAuthConf
    ])

readConfig :: FilePath -> IO (Either String Config)
readConfig filePath = do
    bytesOrError <- try (B.readFile filePath) :: IO (Either IOError B.ByteString)
    return $ case bytesOrError of
        Right bytes -> eitherDecodeStrict bytes
        Left err -> Left . show $ err
