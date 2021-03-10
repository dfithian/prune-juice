import Prelude

import Control.Applicative ((<|>), many)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (execStateT, put)
import Data.Foldable (for_, traverse_)
import Data.List (intercalate)
import Data.Set (Set)
import Data.Text (Text, pack, unpack)
import Data.Traversable (for)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.FilePath.Posix ((</>))
import qualified Data.Set as Set
import qualified Options.Applicative as Opt

import Data.Prune.Cabal (findCabalFiles, parseCabalFiles, parseCabalProjectFile)
import Data.Prune.Dependency (getDependencyByModule)
import Data.Prune.ImportParser (getCompilableUsedDependencies)
import Data.Prune.Stack (parseStackYaml)
import qualified Data.Prune.Types as T

data Opts = Opts
  { optsProjectRoot :: FilePath
  , optsDefaultIgnore :: Bool
  , optsIgnoreList :: [T.DependencyName]
  , optsPackages :: [Text]
  }

defaultIgnoreList :: Set T.DependencyName
defaultIgnoreList = Set.fromList
  [ T.DependencyName "base" -- ignore base because it's needed for info modules etc
  , T.DependencyName "hspec" -- ignore because some packages use hspec discovery
  ]

showIgnoreList :: Set T.DependencyName -> String
showIgnoreList = intercalate ", " . map (unpack . T.unDependencyName) . Set.toList

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
        Opt.long "default-ignore"
          <> Opt.help ("Use the default ignore list (" <> showIgnoreList defaultIgnoreList <> ")") )
      <*> many ( T.DependencyName . pack <$> Opt.strOption (
        Opt.long "ignore"
          <> Opt.metavar "IGNORE"
          <> Opt.help "Dependencies(s) to ignore (overrides the default list)" ) )
      <*> many ( pack <$> Opt.strOption (
        Opt.long "package"
          <> Opt.metavar "PACKAGE"
          <> Opt.help "Package name(s)" ) )

main :: IO ()
main = do
  Opts {..} <- parseArgs

  let ignoreList = if optsDefaultIgnore then defaultIgnoreList else Set.fromList optsIgnoreList

  (buildSystem, packageDirs) <- parseStackYaml (optsProjectRoot </> "stack.yaml")
    <|> parseCabalProjectFile (optsProjectRoot </> "cabal.project")
    <|> findCabalFiles optsProjectRoot
  putStrLn $ "Using build system " <> show buildSystem
  when (buildSystem `elem` [T.CabalProject, T.Cabal]) $
    putStrLn $ "[WARNING] Cabal is not supported"
  packages <- parseCabalFiles packageDirs ignoreList optsPackages

  dependencyByModule <- liftIO $ getDependencyByModule buildSystem packages
  code <- flip execStateT ExitSuccess $ for_ packages $ \T.Package {..} -> do
    baseUsedDependencies <- fmap mconcat . for packageCompilables $ \compilable@T.Compilable {..} -> do
      usedDependencies <- liftIO $ getCompilableUsedDependencies dependencyByModule compilable
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
