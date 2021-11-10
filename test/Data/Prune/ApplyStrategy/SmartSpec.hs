module Data.Prune.ApplyStrategy.SmartSpec where

import Prelude

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.Set as Set

import qualified Data.Prune.Section.Types as T
import qualified Data.Prune.Types as T

-- the module being tested
import Data.Prune.ApplyStrategy.Smart

spec :: Spec
spec = describe "Data.Prune.ApplyStrategy.Smart" $ do
  let base = "  , base <5.0"
      containers = "  , containers"
      mtl = "  , mtl"
      hspec = "  , hspec"
      foo = "  , foo"
      bar = "  , bar"
      nested = T.BuildDependsNestedSection 0 [base, containers, mtl]
      nestedFoo = T.BuildDependsNestedSection 0 [base, containers, mtl, foo]
      nestedBar = T.BuildDependsNestedSection 0 [bar]
      nestedHspec = T.BuildDependsNestedSection 0 [base, containers, mtl, hspec]
      import_ = T.ImportNestedSection 0 [" options"]
      lib = T.TargetSection T.CompilableTypeLibrary Nothing [nested, import_]
      exe = T.TargetSection T.CompilableTypeExecutable (Just (T.CompilableName "foo")) [nestedFoo]
      test = T.TargetSection T.CompilableTypeTest (Just (T.CompilableName "test")) [nestedHspec]
      common = T.CommonSection (T.CommonName "options") [nestedBar]
      allSections = [lib, exe, test, common]

  describe "dependency name regex" $ do
    it "matches a name" $
      matchDependencyName "  base  " `shouldBe` Just (T.DependencyName "base")

    it "matches a name with a version" $
      matchDependencyName "  base <5.0  " `shouldBe` Just (T.DependencyName "base")

  it "strips base out" $ do
    let expected = (T.BuildDependsNestedSection 0 [containers, mtl], mempty)
        actual = stripNestedSection nested (Set.singleton (T.DependencyName "base"))
    actual `shouldBe` expected

  it "strips containers out" $ do
    let expected = (T.BuildDependsNestedSection 0 [base, mtl], mempty)
        actual = stripNestedSection nested (Set.singleton (T.DependencyName "containers"))
    actual `shouldBe` expected

  it "locates a library" $ do
    let expected = T.TargetSection T.CompilableTypeLibrary Nothing [T.BuildDependsNestedSection 0 [containers, mtl], import_]
        actual = stripSections allSections (Set.singleton (T.DependencyName "base")) Nothing
    actual `shouldBe` [expected, exe, test, common]

  it "locates an executable" $ do
    let name = T.CompilableName "foo"
        compilable = T.Compilable name T.CompilableTypeExecutable mempty mempty
        expected = T.TargetSection T.CompilableTypeExecutable (Just name) [T.BuildDependsNestedSection 0 [base, containers, mtl]]
        actual = stripSections allSections (Set.singleton (T.DependencyName "foo")) (Just compilable)
    actual `shouldBe` [lib, expected, test, common]

  it "locates a test suite" $ do
    let name = T.CompilableName "test"
        compilable = T.Compilable name T.CompilableTypeTest mempty mempty
        expected = T.TargetSection T.CompilableTypeTest (Just name) [T.BuildDependsNestedSection 0 [base, containers, mtl]]
        actual = stripSections allSections (Set.singleton (T.DependencyName "hspec")) (Just compilable)
    actual `shouldBe` [lib, exe, expected, common]

  it "locates an import in a library" $ do
    let expected = T.CommonSection (T.CommonName "options") [T.BuildDependsNestedSection 0 []]
        actual = stripSections allSections (Set.singleton (T.DependencyName "bar")) Nothing
    actual `shouldBe` [lib, exe, test, expected]
