module Data.Prune.CabalSpec where

import Prelude

import Data.Foldable (find)
import Data.List (isSuffixOf)
import System.FilePath.TH (fileRelativeToAbsolute)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import qualified Data.Set as Set

import qualified Data.Prune.Types as T

-- the module being tested
import Data.Prune.Cabal

spec :: Spec
spec = describe "Data.Prune.Cabal" $ do
  let cabalProjectFile = $(fileRelativeToAbsolute "../../../cabal.project")
  it "parses .cabal files" $ do
    (_, cabalProject) <- parseCabalProjectFile cabalProjectFile
    [T.Package {..}] <- parseCabalFiles cabalProject []
    packageName `shouldBe` "prune-juice"
    packageBaseDependencies `shouldSatisfy` Set.member (T.DependencyName "base")
    T.Compilable {..} <- maybe (fail "No compilable with the name \"test\"") pure $ find ((==) (T.CompilableName "test") . T.compilableName) packageCompilables
    compilableType `shouldBe` T.CompilableTypeTest
    compilableDependencies `shouldSatisfy` not . Set.null
    compilableFiles `shouldSatisfy` not . Set.null . Set.filter (isSuffixOf "PackageSpec.hs")
