import Prelude

import Test.Hspec (hspec)

import qualified Data.Prune.ApplyStrategy.SmartSpec
import qualified Data.Prune.CabalSpec
import qualified Data.Prune.DependencySpec
import qualified Data.Prune.ImportParserSpec
import qualified Data.Prune.Section.ParserSpec

main :: IO ()
main = hspec $ do
  Data.Prune.CabalSpec.spec
  Data.Prune.DependencySpec.spec
  Data.Prune.ImportParserSpec.spec
  Data.Prune.Section.ParserSpec.spec
  Data.Prune.ApplyStrategy.SmartSpec.spec
