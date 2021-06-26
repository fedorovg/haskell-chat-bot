module Utils where

import Data.Aeson hiding (eitherDecode)
import qualified Data.Aeson.TH as TH (deriveJSON)
import Data.Char (isUpper, toLower)
import Language.Haskell.TH ( Q, Dec, Name )


dropPrefixOptions :: Options
dropPrefixOptions =
  defaultOptions
    { fieldLabelModifier =  camelTo2 '_' . dropPrefix
    , omitNothingFields  = True
    , sumEncoding = UntaggedValue
    }

deriveManyJSON :: Traversable t => Options -> t Name -> Q [Dec]
deriveManyJSON options names = concat <$> mapM (TH.deriveJSON options) names

dropPrefix :: String -> String
dropPrefix []                 = []
dropPrefix (x:xs) | isUpper x = toLower x : xs
                  | otherwise = dropPrefix xs