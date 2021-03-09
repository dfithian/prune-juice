module Data.Prune.Stack where

import Prelude

import qualified Data.ByteString as BS
import qualified Data.Yaml as Yaml

import qualified Data.Prune.Types as T

-- |Parse stack.yaml by file path, filter by explicit package names (if provided), and return the parsed packages.
parseStackYaml :: FilePath -> IO [FilePath]
parseStackYaml stackYamlFile = do
  either (fail . ("Couldn't parse stack.yaml due to " <>) . show) (pure . T.stackYamlPackages) . Yaml.decodeEither' =<< BS.readFile stackYamlFile
