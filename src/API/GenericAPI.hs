module API.GenericAPI where

import qualified Data.Map as M

-- | Authentication defines, how our request a authenticated by the API.
data Authentication
    = Bearer String
    | QueryParam String String
    | None 
    deriving (Show, Eq)

-- | A command, that can be issued by the user
data Command
    = Command String Int
    deriving (Show, Eq)

-- | GenericAPI is a funky attempt a abstracting away most of the REST APIS.
--   This data type is meant to be am instance of th DataAPI class and provides a way fetch data from APIs based on configuration.
data GenericAPI
    = GenericAPI
    { gaUrl :: String
    , gaAuth :: Authentication 
    , gaCommands :: M.Map String Command -- ^ A map from command's name to it's definition
    , gaHelp :: String
    } deriving (Show, Eq)
