-- |Utilities for parsing imports from Haskell source files.
module Data.Prune.ImportParser where

import Prelude

import Control.Applicative ((<|>), optional, some)
import Control.Monad (void)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (pack)
import Data.Traversable (for)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, oneOf, parse)
import Text.Megaparsec.Char (alphaNumChar, char, space, string, symbolChar)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Prune.Types as T

type Parser = Parsec Void String

padded :: Parser a -> Parser a
padded = between space space

quoted :: Parser a -> Parser a
quoted = between (ptoken "\"") (ptoken "\"")

ptoken :: String -> Parser String
ptoken = padded . string

operator :: Parser String
operator = concat <$> sequence [ptoken "(", symbolChars, ptoken ")"]

symbolChars :: Parser String
symbolChars = some (oneOf ("!#$%&*+./<=>?@^|-~:\\" :: String)) <|> some symbolChar

symbol :: Parser String
symbol = padded $ operator <|> some (alphaNumChar <|> oneOf ("._'" :: String))

pkgName :: Parser String
pkgName = some (alphaNumChar <|> char '-')

oneImport :: Parser T.ModuleName
oneImport = void (string "import") *> space
  *> optional (void (string "qualified") *> space)
  *> optional (void (padded (quoted pkgName)) *> space)
  *> (T.ModuleName . pack <$> (symbol <* space))

exposedModules :: Parser (Set T.ModuleName)
exposedModules = void (string "exposed-modules:") *> space
  *> (Set.fromList <$> some (T.ModuleName . pack <$> symbol))

-- |Parse a Haskell source file's imports.
parseFileImports :: FilePath -> IO (Set T.ModuleName)
parseFileImports fp = do
  either (fail . ("Failed to parse imports due to " <>) . show) (pure . Set.fromList) . traverse (parse oneImport fp) . filter (isPrefixOf "import ") . lines
    =<< readFile fp

-- |Parse exposed modules from the `ghc-pkg` field description.
parseExposedModules :: String -> IO (Set T.ModuleName)
parseExposedModules input =
  if null input
    then pure mempty
    else either (\e -> fail $ "Failed to parse exposed modules due to " <> show e <> " original input " <> input) pure $ parse exposedModules "" input

-- |Get the dependencies used by a list of modules imported by a Haskell source file.
getUsedDependencies :: Map T.ModuleName T.DependencyName -> Set T.ModuleName -> Set T.DependencyName
getUsedDependencies dependencyByModule = foldr go mempty . Set.toList
  where
    go next acc = acc <> maybe mempty Set.singleton (Map.lookup next dependencyByModule)

-- |Get the dependencies used by a thing to compile by (1) parsing each source file's imports, (2) getting the
-- dependencies each of those files use, and (3) smooshing all the dependencies together to return.
getCompilableUsedDependencies :: Map T.ModuleName T.DependencyName -> T.Compilable -> IO (Set T.DependencyName)
getCompilableUsedDependencies dependencyByModule T.Compilable {..} = fmap mconcat . for (Set.toList compilableFiles) $ \fp -> do
  moduleNames <- parseFileImports fp
  pure $ getUsedDependencies dependencyByModule moduleNames
