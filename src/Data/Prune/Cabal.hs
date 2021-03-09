module Data.Prune.Cabal where

import Prelude

import Cabal.Project (prjPackages, readProject)
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
import System.Directory (listDirectory)
import System.FilePath.Posix ((</>), isExtensionOf, takeDirectory)
import qualified Data.Set as Set
import qualified Distribution.Types.Benchmark as Benchmark
import qualified Distribution.Types.BuildInfo as BuildInfo
import qualified Distribution.Types.CondTree as CondTree
import qualified Distribution.Types.Dependency as Dependency
import qualified Distribution.Types.Executable as Executable
import qualified Distribution.Types.GenericPackageDescription as GenericPackageDescription
import qualified Distribution.Types.Library as Library
import qualified Distribution.Types.PackageDescription as PackageDescription
import qualified Distribution.Types.PackageId as PackageId
import qualified Distribution.Types.PackageName as PackageName
import qualified Distribution.Types.TestSuite as TestSuite
import qualified Distribution.Types.UnqualComponentName as UnqualComponentName
import qualified Distribution.Verbosity as Verbosity

import qualified Data.Prune.Types as T

-- |Get the dependencies for a thing to compile.
getDependencyNames :: CondTree a [Dependency] b -> Set T.DependencyName
getDependencyNames = Set.fromList . map (T.DependencyName . pack.  PackageName.unPackageName . Dependency.depPkgName) . CondTree.condTreeConstraints

-- |Get the Haskell source files to compile.
getSourceFiles :: FilePath -> BuildInfo -> IO (Set FilePath)
getSourceFiles fp buildInfo = fmap mconcat . for (BuildInfo.hsSourceDirs buildInfo) $ \dir -> do
  allFiles <- listFilesRecursive $ fp </> dir
  pure $ Set.filter (isExtensionOf "hs") allFiles

-- |Parse a library to compile.
getLibraryCompilable :: FilePath -> Set T.DependencyName -> Text -> CondTree a [Dependency] Library -> IO T.Compilable
getLibraryCompilable fp baseDependencies name tree = do
  let compilableName = T.CompilableName name
  sourceFiles <- getSourceFiles fp . Library.libBuildInfo . CondTree.condTreeData $ tree
  pure $ T.Compilable compilableName T.CompilableTypeLibrary (Set.difference (getDependencyNames tree) baseDependencies) sourceFiles

-- |Parse an executable to compile.
getExecutableCompilable :: FilePath -> Set T.DependencyName -> Text -> CondTree a [Dependency] Executable -> IO T.Compilable
getExecutableCompilable fp baseDependencies name tree = do
  let compilableName = T.CompilableName name
  sourceFiles <- getSourceFiles fp . Executable.buildInfo . CondTree.condTreeData $ tree
  pure $ T.Compilable compilableName T.CompilableTypeExecutable (Set.difference (getDependencyNames tree) baseDependencies) sourceFiles

-- |Parse a test to compile.
getTestCompilable :: FilePath -> Set T.DependencyName -> Text -> CondTree a [Dependency] TestSuite -> IO T.Compilable
getTestCompilable fp baseDependencies name tree = do
  let compilableName = T.CompilableName name
  sourceFiles <- getSourceFiles fp . TestSuite.testBuildInfo . CondTree.condTreeData $ tree
  pure $ T.Compilable compilableName T.CompilableTypeTest (Set.difference (getDependencyNames tree) baseDependencies) sourceFiles

-- |Parse a benchmark to compile.
getBenchmarkCompilable :: FilePath -> Set T.DependencyName -> Text -> CondTree a [Dependency] Benchmark -> IO T.Compilable
getBenchmarkCompilable fp baseDependencies name tree = do
  let compilableName = T.CompilableName name
  sourceFiles <- getSourceFiles fp . Benchmark.benchmarkBuildInfo . CondTree.condTreeData $ tree
  pure $ T.Compilable compilableName T.CompilableTypeBenchmark (Set.difference (getDependencyNames tree) baseDependencies) sourceFiles

headMay :: [a] -> Maybe a
headMay = \case
  [] -> Nothing
  x:_ -> Just x

-- |Parse a single cabal file.
parseCabalFile :: FilePath -> IO T.Package
parseCabalFile fp = do
  cabalFile <- maybe (fail $ "No .cabal file found in " <> fp) pure . headMay . filter (isExtensionOf "cabal") =<< listDirectory fp
  genericPackageDescription <- readGenericPackageDescription Verbosity.silent $ fp </> cabalFile
  let baseDependencies = maybe mempty getDependencyNames $ GenericPackageDescription.condLibrary genericPackageDescription
      packageDescription = GenericPackageDescription.packageDescription genericPackageDescription
      packageName = pack . PackageName.unPackageName . PackageId.pkgName . PackageDescription.package $ packageDescription
  libraries         <- traverse (getLibraryCompilable    fp baseDependencies packageName) . maybeToList . GenericPackageDescription.condLibrary $ genericPackageDescription
  internalLibraries <- traverse (getLibraryCompilable    fp baseDependencies <$> pack . UnqualComponentName.unUnqualComponentName . fst <*> snd) . GenericPackageDescription.condSubLibraries $ genericPackageDescription
  executables       <- traverse (getExecutableCompilable fp baseDependencies <$> pack . UnqualComponentName.unUnqualComponentName . fst <*> snd) . GenericPackageDescription.condExecutables $ genericPackageDescription
  tests             <- traverse (getTestCompilable       fp baseDependencies <$> pack . UnqualComponentName.unUnqualComponentName . fst <*> snd) . GenericPackageDescription.condTestSuites $ genericPackageDescription
  benchmarks        <- traverse (getBenchmarkCompilable  fp baseDependencies <$> pack . UnqualComponentName.unUnqualComponentName . fst <*> snd) . GenericPackageDescription.condBenchmarks $ genericPackageDescription
  pure T.Package
    { packageName = packageName
    , packageBaseDependencies = baseDependencies
    , packageCompilables = libraries <> internalLibraries <> executables <> tests <> benchmarks
    }

-- |Parse cabal files by file path, filter by explicit package names (if provided), and return the parsed packages.
parseCabalFiles :: [FilePath] -> [Text] -> IO [T.Package]
parseCabalFiles packageDirs packages = do
  rawPackages <- traverse parseCabalFile packageDirs
  if null packages
    then pure rawPackages
    else pure $ filter (flip elem packages . T.packageName) rawPackages

findCabalFiles :: FilePath -> IO (T.BuildSystem, [FilePath])
findCabalFiles projectRoot = do
  (T.Cabal,) . map takeDirectory . filter (isExtensionOf "cabal") . Set.toList <$> listFilesRecursive projectRoot

-- |Parse cabal.project file by file path.
parseCabalProjectFile :: FilePath -> IO (T.BuildSystem, [FilePath])
parseCabalProjectFile cabalProjectFile = do
  project <- readProject cabalProjectFile
  pure (T.CabalProject, takeDirectory . fst <$> prjPackages project)
