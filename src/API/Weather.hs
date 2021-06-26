{-# LANGUAGE TemplateHaskell #-}

module API.Weather where

import API.DataAPI
import Network.HTTP.Client
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson
import Utils (deriveManyJSON, dropPrefixOptions)


-- | WeatherAPI is a data type that provide Weather info for our Bot.
data WeatherAPI
    = WeatherAPI
    { waToken :: String
    , waBaseUrl :: String
    , waHelpMessage :: String
    } deriving (Show)

-- | Weather is a type used in WeatherAPI's response
data Weather
    = Weather
    { wId :: Int
    , wMain :: String
    } deriving (Show)

-- | Main is a type used in WeatherAPI's response
data Main
    = Main
    { mTemp :: Float
    , mPressure :: Float
    , mHumidity :: Float
    } deriving (Show)

-- | Wind is a type used in WeatherAPI's response
data Wind
    = Wind
    { wSpeed :: Float
    , wDeg :: Float
    } deriving (Show)

-- | WeatherData is a type used in WeatherAPI's response
data WeatherData
    = WeatherData
     { wdWeather :: [Weather]
     , wdMain :: Main
     , wdWind :: Wind
     } deriving (Show)

-- | WeatherError is a type used to represent an Error in WeatherAPI's response
data WeatherError
    = WeatherError
     { weCod :: String
     , weMessage :: String
     } deriving (Show)


$(deriveManyJSON dropPrefixOptions
    [ ''Weather
    , ''Main
    , ''Wind
    , ''WeatherData
    , ''WeatherError
    ])

-- | WeatherAPI is used to fetch weather Data for the bot
instance DataAPI WeatherAPI where
    -- | getdata simply makes a request to the open weather APi and returns a response.
    getData api manager command = do
        let req = parseRequest_ (fullUrl api <> "&q=" <> command <> "&units=metric" )
        response <- httpLbs req manager
        let body = responseBody response
        let mWeatherData = decode body :: Maybe WeatherData
        case mWeatherData of
            Just wd -> pure $ pretty wd command
            _       -> do
                let mErr = decode body :: Maybe WeatherError
                return $ case mErr of
                    Just err -> prettyError api err 
                    _        -> "Weather API is unavailable."


-- | pretty returns Fromated String from a response 
pretty :: WeatherData -> String -> L8.ByteString
pretty d city =
    L8.pack $
        "City: " <> city <> 
        "\nGeneral: " <> show (wMain w) <> 
        "\nTemperature: " <> (show . mTemp . wdMain $ d) <> " C" <> 
        "\nHumidity: " <> (show . mHumidity . wdMain $ d) <> "%" <>
        "\nWind speed: " <> (show . wSpeed . wdWind  $ d) <> "m/s"
    where w = head (wdWeather d)

-- | prettyError returns Fromated String from a WeatherAPI error
prettyError :: WeatherAPI -> WeatherError -> L8.ByteString
prettyError api e = L8.pack ("[WeatherAPI Error]: " <> weMessage e <> "\n" <> waHelpMessage api)

fullUrl :: WeatherAPI -> String
fullUrl api = waBaseUrl api <> "?appid=" <> waToken api
