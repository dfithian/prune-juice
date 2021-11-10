import Prelude

import Control.Applicative ((<|>), many, optional)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
  ( LogLevel(LevelDebug, LevelError, LevelInfo), defaultOutput, logInfo, runLoggingT
  )
import Control.Monad.State (execStateT, put)
import Data.Foldable (foldrM, for_)
import Data.Set (Set)
import Data.Text (Text, pack)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.FilePath.Posix ((</>))
import System.IO (stdout)
import qualified Data.Set as Set
import qualified Options.Applicative as Opt

import Data.Prune.Apply (Apply(ApplySafe, ApplySmart), SomeApply(SomeApply), runApply, writeApply)
import Data.Prune.Cabal (findCabalFiles, parseCabalFiles, parseCabalProjectFile)
import Data.Prune.Confirm (confirm)
import Data.Prune.Dependency (getDependencyByModule)
import Data.Prune.ImportParser (getCompilableUsedDependencies)
import Data.Prune.Section.Parser (readCabalSections)
import Data.Prune.Stack (parseStackYaml)
import qualified Data.Prune.Confirm as Confirm
import qualified Data.Prune.Types as T

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
  , optsStrategy :: T.ApplyStrategy
  }

defaultIgnoreList :: Set T.DependencyName
defaultIgnoreList = Set.fromList
  [ T.DependencyName "base" -- ignore base because it's needed for info modules etc
  , T.DependencyName "hedgehog" -- ignore because some packages use hedgehog discovery
  , T.DependencyName "hspec" -- ignore because some packages use hspec discovery
  , T.DependencyName "hspec-discover" -- ignore because some packages use hspec discovery
  , T.DependencyName "tasty" -- ignore because some packages use tasty discovery
  , T.DependencyName "tasty-hedgehog" -- ignore because some packages use tasty discovery
  , T.DependencyName "tasty-hspec" -- ignore because some packages use tasty discovery
  , T.DependencyName "tasty-hunit" -- ignore because some packages use tasty discovery
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
      <*> Opt.switch (
        Opt.long "apply"
          <> Opt.help "Apply changes" )
      <*> Opt.switch (
        Opt.long "no-verify"
          <> Opt.help "Do not ask for verification when applying (implies --apply)" )
      <*> Opt.option (Opt.maybeReader T.parseApplyStrategy) (
        Opt.long "strategy"
          <> Opt.help ("Strategy to use to apply (one of " <> show T.allApplyStrategies <> ")")
          <> Opt.value T.ApplyStrategySmart
          <> Opt.showDefault )

main :: IO ()
main = do
  Opts {..} <- parseArgs

  let shouldApply = T.validateShouldApply (optsApply, optsNoVerify)

  when (shouldApply == T.ShouldApply) $ do
    putStrLn $ Confirm.warn "Applying results ignores package.yaml files"
    putStrLn $ Confirm.warn "In addition, it could result in unexpected changes to cabal files"
    T.unlessM (confirm "Do you want to continue? (Y/n)") $
      exitWith ExitSuccess

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

      apInit <- case optsStrategy of
        T.ApplyStrategySafe -> pure $ SomeApply $ ApplySafe packageFile packageDescription mempty
        T.ApplyStrategySmart -> do 
          let onFailure str = do
                liftIO $ putStrLn $ Confirm.err $ "Failed to parse cabal sections for " <> packageFile <> " due to " <> str
                put $ ExitFailure 1
                pure mempty
          sections <- either onFailure pure =<< liftIO (readCabalSections packageFile)
          pure $ SomeApply $ ApplySmart packageFile sections mempty

      let addSelf = if optsNoIgnoreSelf then id else Set.insert (T.DependencyName packageName)
          runCompilable compilable@T.Compilable {..} (oldShouldFail, oldUsed, oldStrip) = do
            usedDependencies <- addSelf <$> getCompilableUsedDependencies dependencyByModule compilable
            let (baseUsedDependencies, otherUsedDependencies) = Set.partition (flip Set.member packageBaseDependencies) usedDependencies
                otherUnusedDependencies = Set.difference compilableDependencies otherUsedDependencies
            case Set.null otherUnusedDependencies of
              True -> pure (oldShouldFail, baseUsedDependencies <> oldUsed, oldStrip)
              False -> do
                (shouldFail, strip) <- liftIO $ runApply oldStrip package otherUnusedDependencies (Just compilable) shouldApply
                pure (shouldFail || oldShouldFail, baseUsedDependencies <> oldUsed, strip)
      (targetsShouldFail, targetsUsedDependencies, targetsStrip) <- foldrM runCompilable (False, mempty, apInit) packageCompilables

      let baseUnusedDependencies = Set.difference packageBaseDependencies targetsUsedDependencies
      (finalShouldFail, stripFinal) <- case Set.null baseUnusedDependencies of
        True -> pure (False, targetsStrip)
        False -> liftIO $ runApply targetsStrip package baseUnusedDependencies Nothing shouldApply

      when (targetsShouldFail || finalShouldFail) $ put $ ExitFailure 1
      unless (shouldApply == T.ShouldNotApply) $
        liftIO $ writeApply stripFinal

  exitWith code
