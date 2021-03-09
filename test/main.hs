import Prelude

import Test.Hspec (hspec)

import qualified Data.Prune.CabalSpec
import qualified Data.Prune.ImportParserSpec
import qualified Data.Prune.PackageSpec

main :: IO ()
main = hspec $ do
  Data.Prune.CabalSpec.spec
  Data.Prune.PackageSpec.spec

  Data.Prune.ImportParserSpec.spec
