module Data.Prune.ImportParserSpec where

import Prelude

import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)

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
