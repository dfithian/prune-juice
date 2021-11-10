-- |Description: Utilities for extracting cabal info to the canonical types in "Data.Prune.Types".
module Data.Prune.Cabal where

import Prelude

import Cabal.Project (prjPackages, readProject)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebug)
import Data.Maybe (maybeToList)
import Data.Prune.File (listFilesRecursive)
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Traversable (for)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Types.Benchmark (Benchmark)
import Distribution.Types.BuildInfo (BuildInfo)
import Distribution.Types.CondTree (CondTree)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Executable (Executable)
import Distribution.Types.Library (Library)
import Distribution.Types.TestSuite (TestSuite)
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>), isExtensionOf, takeDirectory, takeFileName)
import qualified Data.Set as Set
import qualified Distribution.Types.Benchmark as Benchmark
import qualified Distribution.Types.BenchmarkInterface as BenchmarkInterface
import qualified Distribution.Types.BuildInfo as BuildInfo
import qualified Distribution.Types.CondTree as CondTree
import qualified Distribution.Types.Executable as Executable
import qualified Distribution.Types.GenericPackageDescription as GenericPackageDescription
import qualified Distribution.Types.Library as Library
import qualified Distribution.Types.PackageDescription as PackageDescription
import qualified Distribution.Types.PackageId as PackageId
import qualified Distribution.Types.PackageName as PackageName
import qualified Distribution.Types.TestSuite as TestSuite
import qualified Distribution.Types.TestSuiteInterface as TestSuiteInterface
import qualified Distribution.Types.UnqualComponentName as UnqualComponentName
import qualified Distribution.Verbosity as Verbosity

import qualified Data.Prune.Types as T

-- |Get the dependencies for a thing to compile.
getDependencyNames :: Set T.DependencyName -> [Dependency] -> Set T.DependencyName
getDependencyNames ignores = flip Set.difference ignores . Set.fromList . map T.mkDependencyName

-- |Get the Haskell source files to compile.
getSourceFiles :: FilePath -> Maybe FilePath -> BuildInfo -> IO (Set FilePath)
getSourceFiles fp mainMay buildInfo = do
  let hsSourceDirs = BuildInfo.hsSourceDirs buildInfo
  allFiles <- case null hsSourceDirs of
    True -> Set.filter (flip elem (takeFileName <$> maybeToList mainMay) . takeFileName) <$> listFilesRecursive fp
    False -> fmap mconcat . for hsSourceDirs $ \dir -> listFilesRecursive $ fp </> dir
  pure $ Set.filter (\fp2 -> any ($ fp2) [isExtensionOf "hs", isExtensionOf "lhs", isExtensionOf "hs-boot"]) allFiles

-- |Parse a library to compile.
getLibraryCompilable :: FilePath -> Set T.DependencyName -> UnqualComponentName -> CondTree a [Dependency] Library -> IO T.Compilable
getLibraryCompilable fp ignores name tree = do
  let compilableName = T.mkCompilableName name
  sourceFiles <- getSourceFiles fp Nothing . Library.libBuildInfo . CondTree.condTreeData $ tree
  pure $ T.Compilable compilableName T.CompilableTypeLibrary (getDependencyNames ignores $ CondTree.condTreeConstraints tree) sourceFiles

-- |Parse an executable to compile.
getExecutableCompilable :: FilePath -> Set T.DependencyName -> UnqualComponentName -> CondTree a [Dependency] Executable -> IO T.Compilable
getExecutableCompilable fp ignores name tree = do
  let compilableName = T.mkCompilableName name
      mainMay = Just . Executable.modulePath . CondTree.condTreeData $ tree
  sourceFiles <- getSourceFiles fp mainMay . Executable.buildInfo . CondTree.condTreeData $ tree
  pure $ T.Compilable compilableName T.CompilableTypeExecutable (getDependencyNames ignores $ CondTree.condTreeConstraints tree) sourceFiles

-- |Parse a test to compile.
getTestCompilable :: FilePath -> Set T.DependencyName -> UnqualComponentName -> CondTree a [Dependency] TestSuite -> IO T.Compilable
getTestCompilable fp ignores name tree = do
  let compilableName = T.mkCompilableName name
      mainMay = case TestSuite.testInterface $ CondTree.condTreeData tree of
        TestSuiteInterface.TestSuiteExeV10 _ exe -> Just exe
        TestSuiteInterface.TestSuiteLibV09 _ _ -> Nothing
        TestSuiteInterface.TestSuiteUnsupported _ -> Nothing
  sourceFiles <- getSourceFiles fp mainMay . TestSuite.testBuildInfo . CondTree.condTreeData $ tree
  pure $ T.Compilable compilableName T.CompilableTypeTest (getDependencyNames ignores $ CondTree.condTreeConstraints tree) sourceFiles

-- |Parse a benchmark to compile.
getBenchmarkCompilable :: FilePath -> Set T.DependencyName -> UnqualComponentName -> CondTree a [Dependency] Benchmark -> IO T.Compilable
getBenchmarkCompilable fp ignores name tree = do
  let compilableName = T.mkCompilableName name
      mainMay = case Benchmark.benchmarkInterface $ CondTree.condTreeData tree of
        BenchmarkInterface.BenchmarkExeV10 _ exe -> Just exe
        BenchmarkInterface.BenchmarkUnsupported _ -> Nothing
  sourceFiles <- getSourceFiles fp mainMay . Benchmark.benchmarkBuildInfo . CondTree.condTreeData $ tree
  pure $ T.Compilable compilableName T.CompilableTypeBenchmark (getDependencyNames ignores $ CondTree.condTreeConstraints tree) sourceFiles

-- |Parse a single cabal file.
parseCabalFile :: FilePath -> Set T.DependencyName -> IO T.Package
parseCabalFile fp ignores = do
  cabalFile <- maybe (fail $ "No .cabal file found in " <> fp) pure . T.headMay . filter (isExtensionOf "cabal") =<< listDirectory fp
  genericPackageDescription <- readGenericPackageDescription Verbosity.silent $ fp </> cabalFile
  let baseDependencies = case GenericPackageDescription.condLibrary genericPackageDescription of
        Just library -> getDependencyNames ignores $ CondTree.condTreeConstraints library
        -- if no library, use set intersection to figure out the common dependencies and use that as the base
        Nothing ->
          let allCabalDependencies :: [Set T.DependencyName]
              allCabalDependencies = fmap (getDependencyNames ignores) . mconcat $
                [ CondTree.condTreeConstraints . snd <$> GenericPackageDescription.condSubLibraries genericPackageDescription
                , CondTree.condTreeConstraints . snd <$> GenericPackageDescription.condExecutables genericPackageDescription
                , CondTree.condTreeConstraints . snd <$> GenericPackageDescription.condTestSuites genericPackageDescription
                , CondTree.condTreeConstraints . snd <$> GenericPackageDescription.condBenchmarks genericPackageDescription
                ]
          in case allCabalDependencies of
            [] -> mempty
            x:xs -> foldr Set.intersection x xs
      packageDescription = GenericPackageDescription.packageDescription genericPackageDescription
      unqualComponentName = UnqualComponentName.mkUnqualComponentName . PackageName.unPackageName . PackageId.pkgName . PackageDescription.package $ packageDescription
      packageName = pack . PackageName.unPackageName . PackageId.pkgName . PackageDescription.package $ packageDescription
  libraries         <- traverse (getLibraryCompilable    fp (ignores <> baseDependencies) unqualComponentName) . maybeToList . GenericPackageDescription.condLibrary $ genericPackageDescription
  internalLibraries <- traverse (getLibraryCompilable    fp (ignores <> baseDependencies) <$> fst <*> snd) . GenericPackageDescription.condSubLibraries $ genericPackageDescription
  executables       <- traverse (getExecutableCompilable fp (ignores <> baseDependencies) <$> fst <*> snd) . GenericPackageDescription.condExecutables $ genericPackageDescription
  tests             <- traverse (getTestCompilable       fp (ignores <> baseDependencies) <$> fst <*> snd) . GenericPackageDescription.condTestSuites $ genericPackageDescription
  benchmarks        <- traverse (getBenchmarkCompilable  fp (ignores <> baseDependencies) <$> fst <*> snd) . GenericPackageDescription.condBenchmarks $ genericPackageDescription
  pure T.Package
    { packageName = packageName
    , packageFile = fp </> cabalFile
    , packageDescription = genericPackageDescription
    , packageBaseDependencies = baseDependencies
    , packageCompilables = libraries <> internalLibraries <> executables <> tests <> benchmarks
    }

-- |Parse cabal files by file path, filter by explicit package names (if provided), and return the parsed packages.
parseCabalFiles :: (MonadIO m, MonadLogger m) => [FilePath] -> Set T.DependencyName -> [Text] -> m [T.Package]
parseCabalFiles packageDirs ignores packages = do
  rawPackages <- liftIO $ traverse (flip parseCabalFile ignores) packageDirs
  let toReturn = if null packages then rawPackages else filter (flip elem packages . T.packageName) rawPackages
  $logDebug $ "Parsed packages " <> pack (show toReturn)
  pure toReturn

-- |Find cabal files under the project root.
findCabalFiles :: FilePath -> IO (T.BuildSystem, [FilePath])
findCabalFiles projectRoot = do
  (T.Cabal,) . map takeDirectory . filter (isExtensionOf "cabal") . Set.toList <$> listFilesRecursive projectRoot

-- |Parse cabal.project file by file path.
parseCabalProjectFile :: FilePath -> IO (T.BuildSystem, [FilePath])
parseCabalProjectFile cabalProjectFile = do
  project <- readProject cabalProjectFile
  pure (T.CabalProject, takeDirectory . fst <$> prjPackages project)
