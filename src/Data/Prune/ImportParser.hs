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

parseFileImports :: FilePath -> IO (Set T.ModuleName)
parseFileImports fp = do
  either (fail . ("Failed to parse imports due to " <>) . show) (pure . Set.fromList) . traverse (parse oneImport fp) . filter (isPrefixOf "import ") . lines
    =<< readFile fp

parseExposedModules :: String -> IO (Set T.ModuleName)
parseExposedModules = either (fail . ("Failed to parse exposed modules due to " <>) . show) pure . parse exposedModules ""

getUsedDependencies :: Map T.ModuleName T.DependencyName -> Set T.ModuleName -> Set T.DependencyName
getUsedDependencies dependencyByModule = foldr go mempty . Set.toList
  where
    go next acc = acc <> maybe mempty Set.singleton (Map.lookup next dependencyByModule)

getCompilableUsedDependencies :: Map T.ModuleName T.DependencyName -> T.Compilable -> IO (Set T.DependencyName)
getCompilableUsedDependencies dependencyByModule T.Compilable {..} = fmap mconcat . for (Set.toList compilableFiles) $ \fp -> do
  moduleNames <- parseFileImports fp
  pure $ getUsedDependencies dependencyByModule moduleNames
