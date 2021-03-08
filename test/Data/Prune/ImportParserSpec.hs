module Data.Prune.ImportParserSpec where

import Prelude

import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import qualified Data.Set as Set

import qualified Data.Prune.Types as T

-- the module being tested
import Data.Prune.ImportParser

spec :: Spec
spec = describe "Data.Prune.ImportParser" $ do
  it "should parse wildcards" $ do
    parse oneImport "" "import Foo.Bar" `shouldBe` Right (T.ModuleName "Foo.Bar")

  it "should parse imports" $ do
    parse oneImport "" "import Foo.Bar (foo, bar)" `shouldBe` Right (T.ModuleName "Foo.Bar")

  it "should parse qualified" $ do
    parse oneImport "" "import qualified Foo.Bar as Baz" `shouldBe` Right (T.ModuleName "Foo.Bar")

  it "should parse package imports" $ do
    parse oneImport "" "import \"foobar\" Foo.Bar ()" `shouldBe` Right (T.ModuleName "Foo.Bar")

  it "should parse something complicated" $ do
    parse oneImport "" "import   qualified   \"foobar\"   Foo.Bar   as   Baz (foo, bar) " `shouldBe` Right (T.ModuleName "Foo.Bar")

  it "should parse dependency name" $ do
    parse dependencyName "" "name: foo-bar" `shouldBe` Right (T.DependencyName "foo-bar")

  it "should parse exposed modules - single line" $ do
    parse exposedModules "" "exposed-modules: Hpack Hpack.Config Hpack.Render Hpack.Yaml"
      `shouldBe` Right (Set.fromList [T.ModuleName "Hpack", T.ModuleName "Hpack.Config", T.ModuleName "Hpack.Render", T.ModuleName "Hpack.Yaml"])

  it "should parse exposed modules - multiline" $ do
    parse exposedModules "" "exposed-modules: Text.Megaparsec Text.Megaparsec.Byte\n\
                                              \Text.Megaparsec.Byte.Lexer Text.Megaparsec.Char\n\
                                              \Text.Megaparsec.Char.Lexer Text.Megaparsec.Debug\n\
                                              \Text.Megaparsec.Error Text.Megaparsec.Error.Builder\n\
                                              \Text.Megaparsec.Internal Text.Megaparsec.Pos Text.Megaparsec.Stream"
      `shouldBe` Right ( Set.fromList
        [ T.ModuleName "Text.Megaparsec", T.ModuleName "Text.Megaparsec.Byte"
        , T.ModuleName "Text.Megaparsec.Byte.Lexer", T.ModuleName "Text.Megaparsec.Char"
        , T.ModuleName "Text.Megaparsec.Char.Lexer", T.ModuleName "Text.Megaparsec.Debug"
        , T.ModuleName "Text.Megaparsec.Error", T.ModuleName "Text.Megaparsec.Error.Builder"
        , T.ModuleName "Text.Megaparsec.Internal", T.ModuleName "Text.Megaparsec.Pos", T.ModuleName "Text.Megaparsec.Stream" ] )
