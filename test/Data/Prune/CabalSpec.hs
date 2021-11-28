module Data.Prune.CabalSpec where

import Prelude

import Control.Monad.Logger (runNoLoggingT)
import Data.Foldable (find)
import Data.List (isSuffixOf)
import System.FilePath.Posix ((</>))
import System.FilePath.TH (fileRelativeToAbsolute)
import Test.Hspec (Spec, describe, it, shouldBe, shouldMatchList, shouldSatisfy)
import qualified Data.Set as Set

import qualified Data.Prune.Types as T

-- the module being tested
import Data.Prune.Cabal

spec :: Spec
spec = describe "Data.Prune.Cabal" $ do
  let cabalProjectFile = $(fileRelativeToAbsolute "../../../cabal.project")
      exampleDirectory = $(fileRelativeToAbsolute "../../../example")

  it "parses .cabal files" $ do
    (_, cabalProject) <- parseCabalProjectFile cabalProjectFile
    [T.Package {..}] <- runNoLoggingT $ parseCabalFiles cabalProject mempty []
    packageName `shouldBe` "prune-juice"
    packageBaseDependencies `shouldSatisfy` Set.member (T.DependencyName "base")
    T.Compilable {..} <- maybe (fail "No compilable with the name \"test\"") pure $ find ((==) (T.CompilableName "test") . T.compilableName) packageCompilables
    compilableType `shouldBe` T.CompilableTypeTest
    compilableDependencies `shouldSatisfy` not . Set.null
    compilableFiles `shouldSatisfy` not . Set.null . Set.filter (isSuffixOf "CabalSpec.hs")

  it "collects the example package" $ do
    T.Package {..} <- parseCabalFile exampleDirectory mempty
    let expectedLib = T.Compilable
          { compilableName = T.CompilableName "example"
          , compilableType = T.CompilableTypeLibrary
          , compilableDependencies = Set.fromList []
          , compilableFiles = Set.fromList [exampleDirectory </> "Data/Example/Lib.hs"]
          }
        expectedTest = T.Compilable
          { compilableName = T.CompilableName "test"
          , compilableType = T.CompilableTypeTest
          , compilableDependencies = Set.fromList [T.DependencyName "example"]
          , compilableFiles = Set.fromList [exampleDirectory </> "Data/Example/Test.hs", exampleDirectory </> "test/main.hs"]
          }
        expectedExe = T.Compilable
          { compilableName = T.CompilableName "example"
          , compilableType = T.CompilableTypeExecutable
          , compilableDependencies = Set.fromList [T.DependencyName "example"]
          , compilableFiles = Set.fromList [exampleDirectory </> "Data/Example/Main.hs", exampleDirectory </> "app/main.hs"]
          }
    packageCompilables `shouldMatchList` [expectedLib, expectedTest, expectedExe]
