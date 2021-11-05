import Prelude

import Control.Applicative ((<|>), many, optional)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
  ( LogLevel(LevelDebug, LevelError, LevelInfo), defaultOutput, logInfo, runLoggingT
  )
import Control.Monad.State (execStateT, put)
import Data.Foldable (for_)
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Traversable (for)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.FilePath.Posix ((</>))
import System.IO (stdout)
import qualified Data.Set as Set
import qualified Options.Applicative as Opt

import Data.Prune.Cabal (findCabalFiles, parseCabalFiles, parseCabalProjectFile)
import Data.Prune.Dependency (getDependencyByModule)
import Data.Prune.ImportParser (getCompilableUsedDependencies)
import Data.Prune.Stack (parseStackYaml)
import qualified Data.Prune.Types as T
import qualified Data.Prune.Unused as Unused

data Opts = Opts
  { optsProjectRoot :: FilePath
  , optsNoDefaultIgnore :: Bool
  , optsNoIgnoreSelf :: Bool
  , optsExtraIgnoreList :: [T.DependencyName]
  , optsPackages :: [Text]
  , optsVerbosity :: T.Verbosity
  , optsBuildSystem :: Maybe T.BuildSystem
  , optsApply :: Bool
  , optsNoVerify :: Bool
  }

defaultIgnoreList :: Set T.DependencyName
defaultIgnoreList = Set.fromList
  [ T.DependencyName "base" -- ignore base because it's needed for info modules etc
  , T.DependencyName "hspec" -- ignore because some packages use hspec discovery
  , T.DependencyName "tasty" -- ignore because some packages use tasty discovery
  ]

verbosityToLogLevel :: T.Verbosity -> Maybe LogLevel
verbosityToLogLevel = \case
  T.Silent -> Nothing
  T.Error -> Just LevelError
  T.Info -> Just LevelInfo
  T.Debug -> Just LevelDebug

parseArgs :: IO Opts
parseArgs = Opt.execParser (Opt.info (Opt.helper <*> parser) $ Opt.progDesc "Prune a Haskell project's dependencies")
  where
    parser = Opts
      <$> Opt.strOption (
        Opt.long "project-root"
          <> Opt.metavar "PROJECT_ROOT"
          <> Opt.help "Project root"
          <> Opt.value "."
          <> Opt.showDefault )
      <*> Opt.switch (
        Opt.long "no-default-ignore"
          <> Opt.help ("Don't use the default ignore list (" <> show (Set.toList defaultIgnoreList) <> ")") )
      <*> Opt.switch (
        Opt.long "no-ignore-self"
          <> Opt.help "Error if an executable doesn't use the library it's defined with" )
      <*> many ( T.DependencyName . pack <$> Opt.strOption (
        Opt.long "extra-ignore"
          <> Opt.metavar "EXTRA_IGNORE"
          <> Opt.help "Dependencies(s) to ignore in addition to the default ignore list" ) )
      <*> many ( pack <$> Opt.strOption (
        Opt.long "package"
          <> Opt.metavar "PACKAGE"
          <> Opt.help "Package name(s)" ) )
      <*> Opt.option (Opt.maybeReader T.parseVerbosity) (
        Opt.long "verbosity"
          <> Opt.metavar "VERBOSITY"
          <> Opt.help ("Set the verbosity level (one of " <> show T.allVerbosities <> ")")
          <> Opt.value T.Error
          <> Opt.showDefault )
      <*> optional ( Opt.option (Opt.maybeReader T.parseBuildSystem) (
        Opt.long "build-system"
          <> Opt.metavar "BUILD_SYSTEM"
          <> Opt.help ("Build system to use instead of inference (one of " <> show T.allBuildSystems <> ")") ) )
      <*> Opt.switch (Opt.long "apply" <> Opt.help "Apply changes (prompts for confirmation unless also specifying --no-verify)")
      <*> Opt.switch (Opt.long "no-verify" <> Opt.help "Skip confirmation when applying changes (implies --apply)")

main :: IO ()
main = do
  Opts {..} <- parseArgs

  apply <- either (\str -> putStrLn str >> exitWith (ExitFailure 1)) pure $
    Unused.validateApply optsApply optsNoVerify

  let ignoreList = Set.fromList optsExtraIgnoreList <> if optsNoDefaultIgnore then mempty else defaultIgnoreList
      logger ma = runLoggingT ma $ case verbosityToLogLevel optsVerbosity of
        Nothing -> \_ _ _ _ -> pure ()
        Just level -> \loc src lvl str -> when (lvl >= level) $ defaultOutput stdout loc src lvl str

  (buildSystem, packageDirs) <- case optsBuildSystem of
    Just T.Stack -> parseStackYaml (optsProjectRoot </> "stack.yaml")
    Just T.CabalProject -> parseCabalProjectFile (optsProjectRoot </> "cabal.project")
    Just T.Cabal -> findCabalFiles optsProjectRoot
    Nothing -> parseStackYaml (optsProjectRoot </> "stack.yaml")
      <|> parseCabalProjectFile (optsProjectRoot </> "cabal.project")
      <|> findCabalFiles optsProjectRoot
  code <- logger $ do
    $logInfo $ "Using build system " <> pack (show buildSystem)
    $logInfo $ "Using ignore list " <> pack (show (Set.toList ignoreList))
    packages <- parseCabalFiles packageDirs ignoreList optsPackages

    dependencyByModule <- getDependencyByModule optsProjectRoot buildSystem packages
    flip execStateT ExitSuccess $ for_ packages $ \package@T.Package {..} -> do
      let addSelf = if optsNoIgnoreSelf then id else Set.insert (T.DependencyName packageName)
      baseUsedDependencies <- fmap mconcat . for packageCompilables $ \compilable@T.Compilable {..} -> do
        usedDependencies <- addSelf <$> getCompilableUsedDependencies dependencyByModule compilable
        let (baseUsedDependencies, otherUsedDependencies) = Set.partition (flip Set.member packageBaseDependencies) usedDependencies
            otherUnusedDependencies = Set.difference compilableDependencies otherUsedDependencies
        unless (Set.null otherUnusedDependencies) $ do
          shouldFail <- liftIO $ Unused.apply package otherUnusedDependencies (Just compilable) apply
          when shouldFail $ put $ ExitFailure 1
        pure baseUsedDependencies
      let baseUnusedDependencies = Set.difference packageBaseDependencies baseUsedDependencies
      unless (Set.null baseUnusedDependencies) $ do
        shouldFail <- liftIO $ Unused.apply package baseUnusedDependencies Nothing apply
        when shouldFail $ put $ ExitFailure 1

  exitWith code
