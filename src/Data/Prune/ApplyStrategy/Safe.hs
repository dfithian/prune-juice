-- |Description: Apply @prune-juice@ to cabal files safely, with the understanding that the file formatting will change.
module Data.Prune.ApplyStrategy.Safe where

import Prelude

import Data.Set (Set)
import Distribution.Types.Benchmark (Benchmark)
import Distribution.Types.BuildInfo (BuildInfo)
import Distribution.Types.CondTree (CondTree)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Executable (Executable)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.Library (Library)
import Distribution.Types.TestSuite (TestSuite)
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import qualified Data.Set as Set
import qualified Distribution.Types.Benchmark as Benchmark
import qualified Distribution.Types.BuildInfo as BuildInfo
import qualified Distribution.Types.CondTree as CondTree
import qualified Distribution.Types.Executable as Executable
import qualified Distribution.Types.GenericPackageDescription as GenericPackageDescription
import qualified Distribution.Types.Library as Library
import qualified Distribution.Types.TestSuite as TestSuite

import qualified Data.Prune.Types as T

-- |Filter out dependencies.
stripDependencies :: Set T.DependencyName -> [Dependency] -> [Dependency]
stripDependencies dependencies = foldr go mempty
  where
    go next accum = case Set.member (T.mkDependencyName next) dependencies of
      True -> accum
      False -> next:accum

-- |Strip dependencies from a single target.
stripBuildInfo :: Set T.DependencyName -> BuildInfo -> BuildInfo
stripBuildInfo dependencies buildInfo = buildInfo
  { BuildInfo.targetBuildDepends = stripDependencies dependencies (BuildInfo.targetBuildDepends buildInfo)
  }

-- |Strip dependencies from a library.
stripLibrary :: Set T.DependencyName -> Library -> Library
stripLibrary dependencies lib = lib
  { Library.libBuildInfo = stripBuildInfo dependencies (Library.libBuildInfo lib)
  }

-- |Strip dependencies from an executable.
stripExecutable :: Set T.DependencyName -> Executable -> Executable
stripExecutable dependencies exe = exe
  { Executable.buildInfo = stripBuildInfo dependencies (Executable.buildInfo exe)
  }

-- |Strip dependencies from a test suite.
stripTestSuite :: Set T.DependencyName -> TestSuite -> TestSuite
stripTestSuite dependencies test = test
  { TestSuite.testBuildInfo = stripBuildInfo dependencies (TestSuite.testBuildInfo test)
  }

-- |Strip dependencies from a benchmark.
stripBenchmark :: Set T.DependencyName -> Benchmark -> Benchmark
stripBenchmark dependencies bench = bench
  { Benchmark.benchmarkBuildInfo = stripBuildInfo dependencies (Benchmark.benchmarkBuildInfo bench)
  }

-- |Strip dependencies from a single target.
stripCondTree :: (b -> b) -> CondTree a [Dependency] b -> CondTree a [Dependency] b
stripCondTree f condTree = condTree
  { CondTree.condTreeData = f (CondTree.condTreeData condTree)
  }

-- |Strip dependencies from multiple targets.
stripCondTrees :: (b -> b) -> T.CompilableName -> [(UnqualComponentName, CondTree a [Dependency] b)] -> [(UnqualComponentName, CondTree a [Dependency] b)]
stripCondTrees f compilableName = foldr go mempty
  where
    go next accum = case compilableName == T.mkCompilableName (fst next) of
      True -> (fst next, stripCondTree f (snd next)):accum
      False -> next:accum

-- |Strip dependencies from a package.
stripGenericPackageDescription :: GenericPackageDescription -> Set T.DependencyName -> Maybe T.Compilable -> GenericPackageDescription
stripGenericPackageDescription genericPackageDescription dependencies = \case
  Nothing -> case GenericPackageDescription.condLibrary genericPackageDescription of
    Nothing -> genericPackageDescription
    Just lib -> genericPackageDescription
      { GenericPackageDescription.condLibrary = Just (stripCondTree (stripLibrary dependencies) lib)
      }
  Just T.Compilable {..} -> case compilableType of
    T.CompilableTypeLibrary -> genericPackageDescription
      { GenericPackageDescription.condSubLibraries = stripCondTrees (stripLibrary dependencies) compilableName (GenericPackageDescription.condSubLibraries genericPackageDescription)
      }
    T.CompilableTypeExecutable -> genericPackageDescription
      { GenericPackageDescription.condExecutables = stripCondTrees (stripExecutable dependencies) compilableName (GenericPackageDescription.condExecutables genericPackageDescription)
      }
    T.CompilableTypeTest -> genericPackageDescription
      { GenericPackageDescription.condTestSuites = stripCondTrees (stripTestSuite dependencies) compilableName (GenericPackageDescription.condTestSuites genericPackageDescription)
      }
    T.CompilableTypeBenchmark -> genericPackageDescription
      { GenericPackageDescription.condBenchmarks = stripCondTrees (stripBenchmark dependencies) compilableName (GenericPackageDescription.condBenchmarks genericPackageDescription)
      }
