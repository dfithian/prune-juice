import Prelude

import Test.Hspec (hspec)

import qualified Data.Prune.CabalSpec
import qualified Data.Prune.DependencySpec
import qualified Data.Prune.ImportParserSpec

main :: IO ()
main = hspec $ do
  Data.Prune.CabalSpec.spec
  Data.Prune.DependencySpec.spec
  Data.Prune.ImportParserSpec.spec
