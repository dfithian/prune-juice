module Data.Prune.Confirm where

import Prelude

import Data.Char (toLower)
import Data.Text (pack, unpack)
import qualified Data.Text.ANSI as Text.ANSI

err, warn, bold :: String -> String
err = unpack . Text.ANSI.bold . Text.ANSI.red . pack
warn = unpack . Text.ANSI.bold . Text.ANSI.yellow . pack
bold = unpack . Text.ANSI.bold . pack

confirm :: String -> IO Bool
confirm msg = do
  putStrLn $ bold msg
  getLine >>= \case
    yes | fmap toLower yes `elem` ["y", "yes"] -> pure True
    no | fmap toLower no `elem` ["n", "no"] -> pure False
    _ -> putStrLn (bold "Please answer Y/n") >> confirm msg
