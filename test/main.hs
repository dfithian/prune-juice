import Prelude

import Test.Hspec (hspec)

import qualified Data.Prune.InternalSpec

main :: IO ()
main = hspec $ do
  Data.Prune.InternalSpec.spec
