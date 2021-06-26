module Logging where

import Control.Monad (when)
import Prelude hiding (log)

-- | Really simple set of logging levels.
data Level
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

-- | A type class to make logger available.
class Monad m => Logger m where
  log :: Level -> Level -> String -> m ()

instance Logger IO where
  log msgLvl appLvl msg =
    when (msgLvl >= appLvl) $
     putStrLn $ "[" <> show msgLvl <> "] " <> msg

-- | Simple shortcut logging funtions
logDebug, logInfo, logWarning :: Logger m => Level -> String -> m ()
logDebug   = log Debug
logInfo    = log Info
logWarning = log Warning

logError :: String -> IO ()
logError msg = putStrLn $ "[Error] " <> msg