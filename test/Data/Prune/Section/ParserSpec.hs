module Data.Prune.Section.ParserSpec where

import Prelude

import Data.Either (isLeft)
import System.FilePath.TH (fileRelativeToAbsolute)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (parse)

import Data.Prune.ApplyStrategy.Smart (stripSections)
import qualified Data.Prune.Section.Types as T
import qualified Data.Prune.Types as T

-- the module being tested
import Data.Prune.Section.Parser

buildDependsNestedSection, importNestedSection, otherNestedSection :: T.NestedSection
buildDependsNestedSection = T.BuildDependsNestedSection 2 ["", "    , base <5.0"]
importNestedSection = T.ImportNestedSection 2 [" foo, bar"]
otherNestedSection = T.OtherNestedSection 2 ["default-language: Haskell2010"]

librarySection, sublibrarySection, executableSection, commonSection, otherSection :: T.Section
librarySection = T.TargetSection T.CompilableTypeLibrary Nothing [importNestedSection, buildDependsNestedSection, otherNestedSection]
sublibrarySection = T.TargetSection T.CompilableTypeLibrary (Just (T.CompilableName "baz")) [importNestedSection, buildDependsNestedSection, otherNestedSection]
executableSection = T.TargetSection T.CompilableTypeExecutable (Just (T.CompilableName "prune-juice")) [buildDependsNestedSection, otherNestedSection]
commonSection = T.CommonSection (T.CommonName "foo") [buildDependsNestedSection, otherNestedSection]
otherSection = T.OtherSection ["name: prune-juice"]

buildDependsNestedExample, importNestedExample, otherNestedExample
  , libraryExample, sublibraryExample, executableExample, commonExample, otherExample :: String
buildDependsNestedExample = unlines
  [ "  build-depends:"
  , "    , base <5.0"
  ]
importNestedExample = unlines
  [ "  import: foo, bar"
  ]
otherNestedExample = unlines
  [ "  default-language: Haskell2010"
  ]
libraryExample = unlines
  [ "library"
  , "  import: foo, bar"
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
commonExample = unlines
  [ "common foo"
  , "  build-depends:"
  , "    , base <5.0"
  , "  default-language: Haskell2010"
  ]
otherExample = unlines
  [ "name: prune-juice"
  ]
sublibraryExample = unlines
  [ "library baz"
  , "  import: foo, bar"
  , "  build-depends:"
  , "    , base <5.0"
  , "  default-language: Haskell2010"
  ]

spec :: Spec
spec = describe "Data.Prune.Section.Parser" $ do
  let cabalFile = $(fileRelativeToAbsolute "../../../../prune-juice.cabal")
      commonFile = $(fileRelativeToAbsolute "../../../fixtures/test.cabal")
      noNewlineFile = $(fileRelativeToAbsolute "../../../fixtures/test-no-newline.cabal")

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

  it "should parse an import nested section" $
    parse nestedSection "" importNestedExample `shouldBe` Right importNestedSection

  it "should parse an other nested section" $
    parse nestedSection "" otherNestedExample `shouldBe` Right otherNestedSection

  it "should parse a library section" $
    parse section "" libraryExample `shouldBe` Right librarySection

  it "should parse a sublibrary section" $
    parse section "" sublibraryExample `shouldBe` Right sublibrarySection

  it "should parse an executable section" $
    parse section "" executableExample `shouldBe` Right executableSection

  it "should parse a common section" $
    parse section "" commonExample `shouldBe` Right commonSection

  it "should parse an other section" $
    parse section "" otherExample `shouldBe` Right otherSection

  it "should parse multiple sections" $
    parse sections "" (libraryExample <> executableExample <> commonExample <> otherExample) `shouldBe` Right [librarySection, executableSection, commonSection, otherSection]

  it "should parse and output the same thing" $ do
    input <- readFile cabalFile
    xs <- either fail pure $ parseCabalSections input
    xs `shouldSatisfy` not . null
    renderCabalSections xs `shouldBe` input

  it "should parse and output the same thing after applying a trivial change" $ do
    input <- readFile cabalFile
    xs <- either fail (\ys -> pure (stripSections ys mempty Nothing)) $ parseCabalSections input
    xs `shouldSatisfy` not . null
    renderCabalSections xs `shouldBe` input

  it "should parse a cabal file with common stanzas" $ do
    input <- readCabalSections commonFile
    input `shouldBe` Right
      [ T.CommonSection (T.CommonName "global-options")
          [ T.OtherNestedSection 2 ["default-language: Haskell2010", ""]
          ]
      , T.CommonSection (T.CommonName "options")
          [ T.ImportNestedSection 2 [" global-options"]
          , T.BuildDependsNestedSection 2 [" base", ""]
          ]
      , T.CommonSection (T.CommonName "global-exe-options")
          [ T.OtherNestedSection 2 ["ghc-options: -threaded", ""]
          ]
      , T.TargetSection T.CompilableTypeLibrary Nothing
          [ T.ImportNestedSection 2 [" options", ""]
          ]
      , T.TargetSection T.CompilableTypeExecutable (Just (T.CompilableName "exe"))
          [ T.ImportNestedSection 2 [" options, global-exe-options"]
          ]
      ]

  it "should parse a cabal file with no trailing newlines" $ do
    input <- readCabalSections noNewlineFile
    input `shouldBe` Right
      [ T.OtherSection ["name: test"]
      ]
