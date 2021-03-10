-- |Load dependencies for a project using `ghc-pkg`.
module Data.Prune.Dependency where

import Prelude hiding (unwords, words)

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text, pack, splitOn, strip, unpack, unwords, words)
import System.Process (readProcess)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Prune.ImportParser (parseDependencyName, parseExposedModules)
import qualified Data.Prune.Types as T

parsePkg :: Text -> IO (T.DependencyName, Set T.ModuleName)
parsePkg s = do
  dependencyName <- parseDependencyName . unpack . unwords . dropWhile (not . (==) "name:") . words . strip $ s
  moduleNames <- parseExposedModules . unpack . unwords . dropWhile (not . (==) "exposed-modules:") . words . strip $ s
  pure (dependencyName, moduleNames)

-- |For the dependencies listed in the specified packages, load `ghc-pkg` and inspect the `exposed-modules` field.
-- Return a map of module to dependency name.
getDependencyByModule :: T.BuildSystem -> [T.Package] -> IO (Map T.ModuleName (Set T.DependencyName))
getDependencyByModule buildSystem packages = do
  let allDependencies = foldMap T.packageBaseDependencies packages <> foldMap T.compilableDependencies (foldMap T.packageCompilables packages)
  rawPkgs <- case buildSystem of
    T.Stack -> readProcess "stack" ["exec", "ghc-pkg", "dump"] ""
    T.CabalProject -> readProcess "cabal" ["v2-exec", "ghc-pkg", "dump"] ""
    T.Cabal -> readProcess "cabal" ["v2-exec", "ghc-pkg", "dump"] ""
  allPkgs <- traverse parsePkg . splitOn "\n---\n" . pack $ rawPkgs
  pure
    . foldr (\(moduleName, dependencyNames) acc -> Map.insertWith (<>) moduleName dependencyNames acc) mempty
    . concatMap (\(dependencyName, moduleNames) -> (, Set.singleton dependencyName) <$> Set.toList moduleNames)
    . filter (flip Set.member allDependencies . fst)
    $ allPkgs
