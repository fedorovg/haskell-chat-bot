module API.DataAPI where

import Network.HTTP.Client
import Data.ByteString.Lazy.Char8 as L8

-- | DataAPI is a simple Class, that generalizes data source for our Bot.
class DataAPI a where
    -- | getData somehow fetches the data used as a response by our Bot.
    getData :: a -> Manager -> String -> IO L8.ByteString
