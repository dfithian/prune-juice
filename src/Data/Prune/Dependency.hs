-- |Load dependencies for a project using `ghc-pkg`.
module Data.Prune.Dependency where

import Prelude hiding (unwords, words)

import Cabal.Config (cfgStoreDir, readConfig)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logError)
import Data.Functor.Identity (runIdentity)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Text (Text, pack, splitOn, strip, unpack, unwords, words)
import System.Directory (doesDirectoryExist)
import System.FilePath.Posix ((</>))
import System.Process (readProcess)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Prune.ImportParser (parseDependencyName, parseExposedModules)
import qualified Data.Prune.Types as T

parsePkg :: (MonadLogger m) => Text -> m (Maybe (T.DependencyName, Set T.ModuleName))
parsePkg s = do
  let dependencyNameInput = unpack . unwords . dropWhile (not . (==) "name:") . words . strip $ s
      moduleNamesInput = unpack . unwords . dropWhile (not . (==) "exposed-modules:") . words . strip $ s
  dependencyNameMay <- case parseDependencyName dependencyNameInput of
    Left err -> do
      $logError $ "Failed to parse dependency name due to " <> pack (show err) <> "; original input " <> pack dependencyNameInput
      pure Nothing
    Right x -> pure x
  moduleNames <- case parseExposedModules moduleNamesInput of
    Left err -> do
      $logError $ "Failed to parse module names due to " <> pack (show err) <> "; original input " <> pack moduleNamesInput
      pure mempty
    Right x -> pure x
  pure $ (, moduleNames) <$> dependencyNameMay

getCabalRawGhcPkgs :: FilePath -> IO String
getCabalRawGhcPkgs projectRoot = do
  cabalConfig <- readConfig
  rawGhcVersion <- readProcess "cabal" ["v2-exec", "ghc", "--", "--numeric-version"] ""
  ghcVersion <- case fmap (unpack . strip) . T.lastMay . words . pack $ rawGhcVersion of
    Nothing -> fail $ "Failed to parse raw GHC version for Cabal from " <> rawGhcVersion
    Just v -> pure v
  let cabalPkgDbDir = (\dir -> dir </> ("ghc-" <> ghcVersion) </> "package.db") . runIdentity . cfgStoreDir $ cabalConfig
      localPkgDbDir = projectRoot </> "dist-newstyle" </> "packagedb" </> ("ghc-" <> ghcVersion)
  defaultPkgs <- readProcess "cabal" ["v2-exec", "ghc-pkg", "dump"] ""
  cabalPkgs <- readProcess "cabal" ["v2-exec", "ghc-pkg", "dump", "--", "--package-db", cabalPkgDbDir] ""
  localPkgs <- doesDirectoryExist localPkgDbDir >>= \case
    True -> Just <$> readProcess "cabal" ["v2-exec", "ghc-pkg", "dump", "--", "--package-db", localPkgDbDir] ""
    False -> pure Nothing
  pure . intercalate "\n---\n" . catMaybes $ [Just defaultPkgs, Just cabalPkgs, localPkgs]

getStackRawGhcPkgs :: IO String
getStackRawGhcPkgs = readProcess "stack" ["exec", "ghc-pkg", "dump"] ""

-- |For the dependencies listed in the specified packages, load `ghc-pkg` and inspect the `exposed-modules` field.
-- Return a map of module to dependency name.
getDependencyByModule :: (MonadIO m, MonadLogger m) => FilePath -> T.BuildSystem -> [T.Package] -> m (Map T.ModuleName (Set T.DependencyName))
getDependencyByModule projectRoot buildSystem packages = do
  let allDependencies = foldMap T.packageBaseDependencies packages <> foldMap T.compilableDependencies (foldMap T.packageCompilables packages)
  rawPkgs <- case buildSystem of
    T.Stack -> liftIO getStackRawGhcPkgs
    T.CabalProject -> liftIO $ getCabalRawGhcPkgs projectRoot
    T.Cabal -> liftIO $ getCabalRawGhcPkgs projectRoot
  allPkgs <- traverse parsePkg . splitOn "\n---\n" . pack $ rawPkgs
  pure
    . foldr (\(moduleName, dependencyNames) acc -> Map.insertWith (<>) moduleName dependencyNames acc) mempty
    . concatMap (\(dependencyName, moduleNames) -> (, Set.singleton dependencyName) <$> Set.toList moduleNames)
    . filter (flip Set.member allDependencies . fst)
    . catMaybes
    $ allPkgs
