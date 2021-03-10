import Prelude

import Control.Applicative ((<|>), many)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (defaultOutput, runLoggingT)
import Control.Monad.State (execStateT, put)
import Data.Foldable (for_, traverse_)
import Data.Set (Set)
import Data.Text (Text, pack, unpack)
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

data Opts = Opts
  { optsProjectRoot :: FilePath
  , optsNoDefaultIgnore :: Bool
  , optsNoIgnoreSelf :: Bool
  , optsExtraIgnoreList :: [T.DependencyName]
  , optsPackages :: [Text]
  , optsVerbose :: Bool
  }

defaultIgnoreList :: Set T.DependencyName
defaultIgnoreList = Set.fromList
  [ T.DependencyName "base" -- ignore base because it's needed for info modules etc
  , T.DependencyName "hspec" -- ignore because some packages use hspec discovery
  , T.DependencyName "tasty" -- ignore because some packages use tasty discovery
  ]

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
      <*> Opt.switch (
        Opt.long "verbose"
          <> Opt.help "Turn on verbose logging"
      )

main :: IO ()
main = do
  Opts {..} <- parseArgs

  let ignoreList = Set.fromList optsExtraIgnoreList <> if optsNoDefaultIgnore then mempty else defaultIgnoreList
      logger ma = runLoggingT ma $ case optsVerbose of
        True -> defaultOutput stdout
        False -> \_ _ _ _ -> pure ()

  (buildSystem, packageDirs) <- parseStackYaml (optsProjectRoot </> "stack.yaml")
    <|> parseCabalProjectFile (optsProjectRoot </> "cabal.project")
    <|> findCabalFiles optsProjectRoot
  putStrLn $ "Using build system " <> show buildSystem
  putStrLn $ "Using ignore list " <> show (Set.toList ignoreList)
  when (buildSystem `elem` [T.CabalProject, T.Cabal]) $
    putStrLn $ "[WARNING] Cabal is not supported"
  code <- logger $ do
    packages <- parseCabalFiles packageDirs ignoreList optsPackages

    dependencyByModule <- liftIO $ getDependencyByModule buildSystem packages
    flip execStateT ExitSuccess $ for_ packages $ \T.Package {..} -> do
      let addSelf = if optsNoIgnoreSelf then id else Set.insert (T.DependencyName packageName)
      baseUsedDependencies <- fmap mconcat . for packageCompilables $ \compilable@T.Compilable {..} -> do
        usedDependencies <- addSelf <$> getCompilableUsedDependencies dependencyByModule compilable
        let (baseUsedDependencies, otherUsedDependencies) = Set.partition (flip Set.member packageBaseDependencies) usedDependencies
            otherUnusedDependencies = Set.difference compilableDependencies otherUsedDependencies
        unless (Set.null otherUnusedDependencies) $ do
          liftIO . putStrLn . unpack $ "Some unused dependencies for " <> pack (show compilableType) <> " " <> T.unCompilableName compilableName <> " in package " <> packageName
          traverse_ (liftIO . putStrLn . unpack . ("  " <>) . T.unDependencyName) $ Set.toList otherUnusedDependencies
          put $ ExitFailure 1
        pure baseUsedDependencies
      let baseUnusedDependencies = Set.difference packageBaseDependencies baseUsedDependencies
      unless (Set.null baseUnusedDependencies) $ do
        liftIO . putStrLn . unpack $ "Some unused base dependencies for package " <> packageName
        liftIO . traverse_ (putStrLn . unpack . ("  " <>) . T.unDependencyName) $ Set.toList baseUnusedDependencies
        put $ ExitFailure 1
  exitWith code
