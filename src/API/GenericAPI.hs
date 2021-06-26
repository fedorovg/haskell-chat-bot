module API.GenericAPI where

import API.DataAPI
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Internal as BI
import Network.HTTP.Client
import Data.Aeson
import qualified Data.Yaml as YML
import Data.Functor ((<&>))
import Data.List
import qualified Config as C

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

instance DataAPI GenericAPI where
  -- | getData parses a text command it receives and response with either a result of that command's invokation or a help message
  getData api manager message = do
    let (command, params) = parseCommand api message
    case command of
        (Command "help" 0) -> return . L8.pack . gaHelp $ api
        c                   -> do
            let url = makeUrl api c params
            print url
            initialReq <- parseRequest url
            let req = applyAuth initialReq (gaAuth api) 
            response <- httpLbs req manager
            let mVal = decode . responseBody $ response :: Maybe Value
            let res = fromMaybe "[GenericAPI ERROR] Configured commands are invalid." (mVal <&> YML.encode)
            return $ L8.fromStrict res

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

-- | applyAuth adds Authorisation header to a http request, if they are requred
applyAuth :: Request -> Authentication -> Request
applyAuth r (Bearer token) = r {requestHeaders = [("Authorization", BI.packChars $ "Bearer " <> token)]}
applyAuth r _ = r

createGenericApi :: C.ApiConfig -> GenericAPI
createGenericApi (C.GenericApiConfig url auth commands help) = GenericAPI url (authFromConfig auth) (M.fromList $ map commandFromConf commands) help 

commandFromConf :: C.CommandConf -> (String, Command)
commandFromConf (C.CommandConf key name pars) = (key, Command name pars)

authFromConfig :: C.ApiAuthConf -> Authentication
authFromConfig = \case
    C.None -> None
    C.Bearer t -> Bearer t
    C.QueryParam n v -> QueryParam n v