-- |Types for pruning.
module Data.Prune.Types where

import Prelude

import Data.Aeson ((.:), FromJSON, parseJSON, withObject)
import Data.Set (Set)
import Data.Text (Text, pack, unpack)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import qualified Distribution.Types.Dependency as Dependency
import qualified Distribution.Types.PackageName as PackageName
import qualified Distribution.Types.UnqualComponentName as UnqualComponentName

data BuildSystem = Stack | CabalProject | Cabal
  deriving (Eq, Ord, Bounded, Enum)

instance Show BuildSystem where
  show = \case
    Stack -> "stack"
    CabalProject -> "cabal-project"
    Cabal -> "cabal"

parseBuildSystem :: String -> Maybe BuildSystem
parseBuildSystem = \case
  "stack" -> Just Stack
  "cabal-project" -> Just CabalProject
  "cabal" -> Just Cabal
  _ -> Nothing

allBuildSystems :: [BuildSystem]
allBuildSystems = [minBound..maxBound]

data Verbosity = Silent | Error | Info | Debug
  deriving (Eq, Ord, Bounded, Enum)

instance Show Verbosity where
  show = \case
    Silent -> "silent"
    Error -> "error"
    Info -> "info"
    Debug -> "debug"

parseVerbosity :: String -> Maybe Verbosity
parseVerbosity = \case
  "silent" -> Just Silent
  "error" -> Just Error
  "info" -> Just Info
  "debug" -> Just Debug
  _ -> Nothing

allVerbosities :: [Verbosity]
allVerbosities = [minBound..maxBound]

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
  , packageFile :: FilePath
  , packageDescription :: GenericPackageDescription
  -- ^ The path to the config file.
  , packageBaseDependencies :: Set DependencyName
  -- ^ The list of common dependencies.
  , packageCompilables :: [Compilable]
  -- ^ The things to compile in the package.
  }
  deriving (Eq, Show)

data StackYaml = StackYaml
  { stackYamlPackages :: [FilePath]
  -- ^ The list of packages in stack.yaml. FIXME not every package is this way.
  }
  deriving (Eq, Ord, Show)

instance FromJSON StackYaml where
  parseJSON = withObject "StackYaml" $ \obj ->
    StackYaml
      <$> obj .: "packages"

data ShouldApply = ShouldNotApply | ShouldApply | ShouldApplyNoVerify
  deriving (Eq, Ord, Bounded, Enum)

instance Show ShouldApply where
  show = \case
    ShouldNotApply -> "no-apply"
    ShouldApply -> "apply"
    ShouldApplyNoVerify -> "apply-no-verify"

validateShouldApply :: (Bool, Bool) -> ShouldApply
validateShouldApply = \case
  (_, True) -> ShouldApplyNoVerify
  (True, False) -> ShouldApply
  (False, False) -> ShouldNotApply

allApply :: [ShouldApply]
allApply = [minBound..maxBound]

data ApplyStrategy = ApplyStrategySafe | ApplyStrategySmart
  deriving (Eq, Ord, Bounded, Enum)

instance Show ApplyStrategy where
  show = \case
    ApplyStrategySafe -> "safe"
    ApplyStrategySmart -> "smart"

parseApplyStrategy :: String -> Maybe ApplyStrategy
parseApplyStrategy = \case
  "safe" -> Just ApplyStrategySafe
  "smart" -> Just ApplyStrategySmart
  _ -> Nothing

allApplyStrategies :: [ApplyStrategy]
allApplyStrategies = [minBound..maxBound]

mkDependencyName :: Dependency -> DependencyName
mkDependencyName = DependencyName . pack . PackageName.unPackageName . Dependency.depPkgName

mkCompilableName :: UnqualComponentName -> CompilableName
mkCompilableName = CompilableName . pack . UnqualComponentName.unUnqualComponentName

headMay :: [a] -> Maybe a
headMay = \case
  [] -> Nothing
  x:_ -> Just x

lastMay :: [a] -> Maybe a
lastMay = headMay . reverse

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do x <- b; if x then t else f

whenM :: Monad m => m Bool -> m () -> m ()
whenM b t = ifM b t (pure ())

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b = whenM (not <$> b)
