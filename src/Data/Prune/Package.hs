-- |Utilities for package.yaml parsing.
module Data.Prune.Package where

import Prelude

import Data.Map (Map)
import Data.Prune.File (listFilesRecursive)
import Data.Set (Set)
import Data.Text (Text, pack)
import Data.Traversable (for)
import Hpack.Config
  ( Section, decodeOptionsTarget, decodeResultPackage, defaultDecodeOptions, packageBenchmarks
  , packageConfig, packageExecutables, packageInternalLibraries, packageLibrary, packageName
  , packageTests, readPackageConfig, sectionDependencies, sectionSourceDirs, unDependencies
  )
import System.FilePath.Posix ((</>), isExtensionOf)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Prune.Types as T

-- |Get the dependencies for a thing to compile.
getSectionDependencyNames :: Set T.DependencyName -> Section a -> Set T.DependencyName
getSectionDependencyNames ignores = flip Set.difference ignores . Set.fromList . map (T.DependencyName . pack) . Map.keys . unDependencies . sectionDependencies

-- |Get the Haskell source files to compile.
getSectionFiles :: FilePath -> Section a -> IO (Set FilePath)
getSectionFiles fp section = fmap mconcat . for (sectionSourceDirs section) $ \dir -> do
  allFiles <- listFilesRecursive $ fp </> dir
  pure $ Set.filter (isExtensionOf "hs") allFiles

-- |Parse a thing to compile.
getSectionCompilables :: FilePath -> T.CompilableType -> Set T.DependencyName -> Map String (Section a) -> IO [T.Compilable]
getSectionCompilables fp typ ignores sections = for (Map.toList sections) $ \(name, section) -> do
  sourceFiles <- getSectionFiles fp section
  pure $ T.Compilable (T.CompilableName (pack name)) typ (getSectionDependencyNames ignores section) sourceFiles

-- |Parse a single package.yaml file.
parsePackageYaml :: FilePath -> Set T.DependencyName -> IO T.Package
parsePackageYaml fp ignores = do
  package <- either fail (pure . decodeResultPackage) =<< readPackageConfig (defaultDecodeOptions { decodeOptionsTarget = fp </> packageConfig })
  let baseDependencies = maybe mempty (getSectionDependencyNames ignores) $ packageLibrary package
  libraries         <- getSectionCompilables fp T.CompilableTypeLibrary    (ignores <> baseDependencies) $ maybe mempty (Map.singleton (packageName package)) $ packageLibrary package
  internalLibraries <- getSectionCompilables fp T.CompilableTypeLibrary    (ignores <> baseDependencies) $ packageInternalLibraries package
  executables       <- getSectionCompilables fp T.CompilableTypeExecutable (ignores <> baseDependencies) $ packageExecutables package
  tests             <- getSectionCompilables fp T.CompilableTypeTest       (ignores <> baseDependencies) $ packageTests package
  benchmarks        <- getSectionCompilables fp T.CompilableTypeBenchmark  (ignores <> baseDependencies) $ packageBenchmarks package
  pure T.Package
    { packageName = pack $ packageName package
    , packageBaseDependencies = baseDependencies
    , packageCompilables = libraries <> internalLibraries <> executables <> tests <> benchmarks
    }

-- |Parse package.yaml files by file path, filter by explicit package names (if provided), and return the parsed packages.
parsePackageYamls :: [FilePath] -> Set T.DependencyName -> [Text] -> IO [T.Package]
parsePackageYamls packageDirs ignores packages = do
  rawPackages <- traverse (flip parsePackageYaml ignores) packageDirs
  if null packages
    then pure rawPackages
    else pure $ filter (flip elem packages . T.packageName) rawPackages
