module API.GenericAPI where

import qualified Data.Map as M
import qualified Config as C 
import Data.List

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

-- | parseCommand reads through the text message sent by the user and trys to match it with a predefined command. Returns help message command in case of failure 
parseCommand :: GenericAPI -> String -> (Command, [String])
parseCommand (GenericAPI _ _ commands _) = f . words
    where f [] = (Command "help" 0, [])
          f [command] = case M.lookup command commands of
              Just c@(Command name 0) -> (c, [])
              _                        -> (Command "help" 0, [])
          f (command : params) = case M.lookup command commands of
              Just c@(Command name cParams) -> if cParams == length params then (c, params) else (Command "help" 0, [])
              _                             -> (Command "help" 0, [])

-- | makeUrl converts api and command pair into a string use to make an HTTP request
makeUrl :: GenericAPI -> Command -> [String] -> String
makeUrl (GenericAPI url auth _ _) (Command name _) [] = url <> "/" <> name <> authToUrl auth
makeUrl (GenericAPI url auth _ _) (Command name paramNames) params = url <> "/" <> name <> "/" <> combineNamesAndValues params <> authToUrl auth
    where combineNamesAndValues params = intercalate "/" params 

-- | authToUrl adds a query parameter to the url, if Api requires such Authentication
authToUrl :: Authentication -> String
authToUrl (QueryParam name val) = "?" <> name <> "=" <> val
authToUrl _ = ""

createGenericApi :: C.ApiConfig -> GenericAPI
createGenericApi (C.GenericApiConfig url auth commands help) = GenericAPI url (authFromConfig auth) (M.fromList $ map commandFromConf commands) help 

commandFromConf :: C.CommandConf -> (String, Command)
commandFromConf (C.CommandConf key name pars) = (key, Command name pars)

authFromConfig :: C.ApiAuthConf -> Authentication
authFromConfig = \case
    C.None -> None
    C.Bearer t -> Bearer t
    C.QueryParam n v -> QueryParam n v