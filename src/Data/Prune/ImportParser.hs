-- |Utilities for parsing imports from Haskell source files.
module Data.Prune.ImportParser (getCompilableUsedDependencies, oneImport) where

import Prelude

import Control.Applicative ((<|>), optional, some)
import Control.Arrow (left)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebug, logError)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
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

symbol' :: Parser String
symbol' = operator <|> some (alphaNumChar <|> oneOf ("._'" :: String))

symbol :: Parser String
symbol = padded symbol'

pkgName :: Parser String
pkgName = some (alphaNumChar <|> char '-')

oneImport :: Parser T.ModuleName
oneImport = void (string "import") *> space
  *> optional (between "{-#" "#-}" (space *> void (string "SOURCE") *> space) *> space)
  *> optional (void (string "qualified") *> space)
  *> optional (void (padded (quoted pkgName)) *> space)
  *> (T.ModuleName . pack <$> (symbol <* space))

-- |Parse a Haskell source file's imports.
parseFileImports :: FilePath -> IO (Either String (Set T.ModuleName))
parseFileImports fp = do
  left show . fmap Set.fromList . traverse (parse oneImport fp) . filter (isPrefixOf "import ") . lines
    <$> readFile fp

-- |Get the dependencies used by a list of modules imported by a Haskell source file.
getUsedDependencies :: Map T.ModuleName (Set T.DependencyName) -> Set T.ModuleName -> Set T.DependencyName
getUsedDependencies dependencyByModule = foldr go mempty . Set.toList
  where
    go next acc = acc <> fromMaybe mempty (Map.lookup next dependencyByModule)

-- |Get the dependencies used by a thing to compile by (1) parsing each source file's imports, (2) getting the
-- dependencies each of those files use, and (3) smooshing all the dependencies together to return.
getCompilableUsedDependencies :: (MonadIO m, MonadLogger m) => Map T.ModuleName (Set T.DependencyName) -> T.Compilable -> m (Set T.DependencyName)
getCompilableUsedDependencies dependencyByModule T.Compilable {..} = fmap mconcat . for (Set.toList compilableFiles) $ \fp -> do
  liftIO (parseFileImports fp) >>= \case
    Left err -> do
      $logError $ "Failed to parse imports for " <> pack fp <> " due to " <> pack (show err)
      pure mempty
    Right moduleNames -> do
      $logDebug $ "Got module names for " <> pack fp <> ": " <> pack (show moduleNames)
      let usedDependencies = getUsedDependencies dependencyByModule moduleNames
      $logDebug $ "Got dependency names for " <> pack fp <> ": " <> pack (show usedDependencies)
      pure usedDependencies
