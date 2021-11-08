module Data.Prune.Section.ParserSpec where

import Prelude

import Data.Either (isLeft)
import System.FilePath.TH (fileRelativeToAbsolute)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (parse)

import qualified Data.Prune.Section.Types as T
import qualified Data.Prune.Types as T

-- the module being tested
import Data.Prune.Section.Parser

buildDependsNestedSection, otherNestedSection :: T.NestedSection
buildDependsNestedSection = T.BuildDependsNestedSection 2 ["", "    , base <5.0"]
otherNestedSection = T.OtherNestedSection 2 ["default-language: Haskell2010"]

librarySection, executableSection, otherSection :: T.Section
librarySection = T.TargetSection T.CompilableTypeLibrary Nothing [buildDependsNestedSection, otherNestedSection]
executableSection = T.TargetSection T.CompilableTypeExecutable (Just (T.CompilableName "prune-juice")) [buildDependsNestedSection, otherNestedSection]
otherSection = T.OtherSection ["name: prune-juice"]

buildDependsNestedExample, otherNestedExample, libraryExample, executableExample, otherExample :: String
buildDependsNestedExample = unlines
  [ "  build-depends:"
  , "    , base <5.0"
  ]
otherNestedExample = unlines
  [ "  default-language: Haskell2010"
  ]
libraryExample = unlines
  [ "library"
  , "  build-depends:"
  , "    , base <5.0"
  , "  default-language: Haskell2010"
  ]
executableExample = unlines
  [ "executable prune-juice"
  , "  build-depends:"
  , "    , base <5.0"
  , "  default-language: Haskell2010"
  ]
otherExample = unlines
  [ "name: prune-juice"
  ]

spec :: Spec
spec = describe "Data.Prune.Section.Parser" $ do
  let cabalFile = $(fileRelativeToAbsolute "../../../../prune-juice.cabal")

  it "should parse a target name" $
    parse targetName "" "prune-juice" `shouldBe` Right (T.CompilableName "prune-juice")

  it "should parse the rest of the line" $
    parse restOfLine "" "foo\n" `shouldBe` Right "foo"

  it "should parse an indented line" $
    parse (indentedLine 0) "" " foo\n" `shouldBe` Right " foo"

  it "should not parse a non-indented line" $
    parse (indentedLine 0) "" "foo\n" `shouldSatisfy` isLeft

  it "should parse indented lines" $
    parse (indentedLines 0) "" "foo\n bar\nbaz\n" `shouldBe` Right ["foo", " bar"]

  it "should parse a build depends nested section" $
    parse nestedSection "" buildDependsNestedExample `shouldBe` Right buildDependsNestedSection

  it "should parse an other nested section" $
    parse nestedSection "" otherNestedExample `shouldBe` Right otherNestedSection

  it "should parse a library section" $
    parse section "" libraryExample `shouldBe` Right librarySection

  it "should parse an executable section" $
    parse section "" executableExample `shouldBe` Right executableSection

  it "should parse an other section" $
    parse section "" otherExample `shouldBe` Right otherSection

  it "should parse multiple sections" $
    parse sections "" (libraryExample <> executableExample <> otherExample) `shouldBe` Right [librarySection, executableSection, otherSection]

  it "should parse and output the same thing" $ do
    input <- readFile cabalFile
    let output = either (const "") renderCabalSections $ parseCabalSections input
    output `shouldBe` input
