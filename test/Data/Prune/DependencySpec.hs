module Data.Prune.DependencySpec where

import Prelude

import Control.Monad.Logger (runNoLoggingT)
import Data.FileEmbed (embedFile)
import Data.Text.Encoding (decodeUtf8)
import System.FilePath.TH (fileRelativeToAbsolute, fileRelativeToAbsoluteStr)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, xit)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Prune.Cabal (parseCabalFile)
import qualified Data.Prune.Types as T

-- the module being tested
import Data.Prune.Dependency

spec :: Spec
spec = describe "Data.Prune.Dependency" $ do
  let thisFile = $(fileRelativeToAbsolute "DependencySpec.hs")
      projectRoot = $(fileRelativeToAbsolute "../../../")

  it "should parse the empty string" $ do
    pkgs <- runNoLoggingT $ parsePkg ""
    pkgs `shouldBe` Nothing

  it "should parse a ghc-pkg module" $ do
    let rawModuleNames =
          [ "Data.Text"
          , "Data.Text.Array"
          , "Data.Text.Encoding"
          , "Data.Text.Encoding.Error"
          , "Data.Text.Foreign"
          , "Data.Text.IO"
          , "Data.Text.Internal"
          , "Data.Text.Internal.Builder"
          , "Data.Text.Internal.Builder.Functions"
          , "Data.Text.Internal.Builder.Int.Digits"
          , "Data.Text.Internal.Builder.RealFloat.Functions"
          , "Data.Text.Internal.Encoding.Fusion"
          , "Data.Text.Internal.Encoding.Fusion.Common"
          , "Data.Text.Internal.Encoding.Utf16"
          , "Data.Text.Internal.Encoding.Utf32"
          , "Data.Text.Internal.Encoding.Utf8"
          , "Data.Text.Internal.Functions"
          , "Data.Text.Internal.Fusion"
          , "Data.Text.Internal.Fusion.CaseMapping"
          , "Data.Text.Internal.Fusion.Common"
          , "Data.Text.Internal.Fusion.Size"
          , "Data.Text.Internal.Fusion.Types"
          , "Data.Text.Internal.IO"
          , "Data.Text.Internal.Lazy"
          , "Data.Text.Internal.Lazy.Encoding.Fusion"
          , "Data.Text.Internal.Lazy.Fusion"
          , "Data.Text.Internal.Lazy.Search"
          , "Data.Text.Internal.Private"
          , "Data.Text.Internal.Read"
          , "Data.Text.Internal.Search"
          , "Data.Text.Internal.Unsafe"
          , "Data.Text.Internal.Unsafe.Char"
          , "Data.Text.Internal.Unsafe.Shift"
          , "Data.Text.Lazy"
          , "Data.Text.Lazy.Builder"
          , "Data.Text.Lazy.Builder.Int"
          , "Data.Text.Lazy.Builder.RealFloat"
          , "Data.Text.Lazy.Encoding"
          , "Data.Text.Lazy.IO"
          , "Data.Text.Lazy.Internal"
          , "Data.Text.Lazy.Read"
          , "Data.Text.Read"
          , "Data.Text.Unsafe"
          ]
    pkgs <- runNoLoggingT $ parsePkg $ decodeUtf8 $(embedFile =<< fileRelativeToAbsoluteStr "../../fixtures/ghc-pkg.txt")
    pkgs `shouldBe` Just (T.DependencyName "text", Set.fromList . fmap T.ModuleName $ rawModuleNames)

  -- this test doesn't succeed under `stack test` presumably because stack is using a different version of cabal
  xit "loads dependencies for this testing module using cabal" $ do
    package <- parseCabalFile projectRoot mempty
    thisModule <- runNoLoggingT $ getDependencyByModule thisFile T.Cabal [package]
    Set.unions (Map.elems thisModule) `shouldSatisfy` Set.member (T.DependencyName "base")

  it "loads dependencies for this testing module using stack" $ do
    package <- parseCabalFile projectRoot mempty
    thisModule <- runNoLoggingT $ getDependencyByModule thisFile T.Stack [package]
    Set.unions (Map.elems thisModule) `shouldSatisfy` Set.member (T.DependencyName "base")
