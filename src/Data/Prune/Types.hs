module Data.Prune.Types where

import Prelude

import Data.Aeson ((.:), FromJSON, parseJSON, withObject)
import Data.Set (Set)
import Data.Text (Text)

data CompilableType
  = CompilableTypeLibrary
  | CompilableTypeExecutable
  | CompilableTypeTest
  | CompilableTypeBenchmark
  deriving (Eq, Ord)

instance Show CompilableType where
  show = \case
    CompilableTypeLibrary -> "library"
    CompilableTypeExecutable -> "executable"
    CompilableTypeTest -> "test"
    CompilableTypeBenchmark -> "benchmark"

newtype CompilableName = CompilableName { unCompilableName :: Text }
  deriving (Eq, Ord, Show)

data DependencyName = DependencyName { unDependencyName :: Text }
  deriving (Eq, Ord, Show)

data ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Ord, Show)

data Compilable = Compilable
  { compilableName :: CompilableName
  , compilableType :: CompilableType
  , compilableDependencies :: Set DependencyName
  , compilableFiles :: Set FilePath
  }
  deriving (Eq, Ord, Show)

data Package = Package
  { packageName :: Text
  , packageBaseDependencies :: Set DependencyName
  , packageCompilables :: [Compilable]
  }
  deriving (Eq, Ord, Show)

data StackYaml = StackYaml
  { stackYamlPackages :: [FilePath]
  -- ^ FIXME not every package is this way.
  }
  deriving (Eq, Ord, Show)

instance FromJSON StackYaml where
  parseJSON = withObject "StackYaml" $ \obj ->
    StackYaml
      <$> obj .: "packages"
