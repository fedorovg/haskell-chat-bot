module Main where

import System.Environment (getArgs)
import Lib (start)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> start path
    _      -> start "config/config.json"
