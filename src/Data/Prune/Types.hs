-- |Types for pruning.
module Data.Prune.Types where

import Prelude

import Data.Aeson ((.:), FromJSON, parseJSON, withObject)
import Data.Set (Set)
import Data.Text (Text, unpack)

data BuildSystem = Stack | CabalProject | Cabal
  deriving (Eq, Ord, Show)

-- |The type of the thing to compile.
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

-- |The name of the thing to compile.
newtype CompilableName = CompilableName { unCompilableName :: Text }
  deriving (Eq, Ord)

instance Show CompilableName where
  show = unpack . unCompilableName

-- |The name of the dependency as listed in package.yaml
data DependencyName = DependencyName { unDependencyName :: Text }
  deriving (Eq, Ord)

instance Show DependencyName where
  show = unpack . unDependencyName

-- |A qualified module name, like `Foo.Bar`
data ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Ord)

instance Show ModuleName where
  show = unpack . unModuleName

-- |A thing to compile.
data Compilable = Compilable
  { compilableName :: CompilableName
  , compilableType :: CompilableType
  , compilableDependencies :: Set DependencyName
  -- ^ The list of dependencies less the common dependencies.
  , compilableFiles :: Set FilePath
  -- ^ The files under `source-dirs`.
  }
  deriving (Eq, Ord, Show)

data Package = Package
  { packageName :: Text
  , packageBaseDependencies :: Set DependencyName
  -- ^ The list of common dependencies.
  , packageCompilables :: [Compilable]
  -- ^ The things to compile in the package.
  }
  deriving (Eq, Ord, Show)

data StackYaml = StackYaml
  { stackYamlPackages :: [FilePath]
  -- ^ The list of packages in stack.yaml. FIXME not every package is this way.
  }
  deriving (Eq, Ord, Show)

instance FromJSON StackYaml where
  parseJSON = withObject "StackYaml" $ \obj ->
    StackYaml
      <$> obj .: "packages"
