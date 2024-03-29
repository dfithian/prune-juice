-- |Description: Apply @prune-juice@ to cabal files, attempting to overwrite /only/ the dependencies portions.
module Data.Prune.ApplyStrategy.Smart where

import Prelude

import Control.Arrow (second)
import Data.Function (fix)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Text (pack, splitOn, unpack)
import Text.Regex (Regex, matchRegex, mkRegex)
import qualified Data.Set as Set

import qualified Data.Prune.Section.Types as T
import qualified Data.Prune.Types as T

-- |A type for which target we're trying to strip.
data StripTarget
  = StripTargetBaseLibrary
  -- ^ The base library
  | StripTargetCompilable T.Compilable
  -- ^ Any @library@, @executable@, @test-suite@, @benchmark@, etc stanza.
  | StripTargetCommonStanza (Set T.CommonName)
  -- ^ Any @common@ stanza matching the set.

-- |Regex for dependency names like @base <5.0@.
dependencyNameRegex :: Regex
dependencyNameRegex = mkRegex "^ *([a-zA-Z0-9\\-]+).*$"

-- |Parse a dependency name from a string.
matchDependencyName :: String -> Maybe T.DependencyName
matchDependencyName str = Just . T.DependencyName . pack =<< T.headMay =<< matchRegex dependencyNameRegex str

-- |Strip matching dependencies from a single line.
stripOneBuildDepends :: String -> Set T.DependencyName -> Maybe String
stripOneBuildDepends input dependencies =
  let output = intercalate "," . mapMaybe go . fmap unpack . splitOn "," . pack $ input
  in case not (null output) && all ((==) ' ') output of
      True -> Nothing
      False -> Just output
  where
    go x = case matchDependencyName x of
      Nothing -> Just x
      Just dep -> case Set.member dep dependencies of
        True -> Nothing
        False -> Just x

-- |Strip matching dependencies from a @build-depends@ section.
stripBuildDepends :: [String] -> Set T.DependencyName -> [String]
stripBuildDepends buildDepends dependencies = mapMaybe (\x -> stripOneBuildDepends x dependencies) buildDepends

-- |Strip matching dependencies from a nested section.
stripNestedSection :: T.NestedSection -> Set T.DependencyName -> (T.NestedSection, Set T.CommonName)
stripNestedSection nested dependencies = case nested of
  T.BuildDependsNestedSection numSpaces buildDepends -> (T.BuildDependsNestedSection numSpaces (stripBuildDepends buildDepends dependencies), mempty)
  T.ImportNestedSection numSpaces imports ->
    let common = Set.fromList $ T.CommonName <$> mconcat (fmap (splitOn "," . pack) (words (unwords imports)))
    in (T.ImportNestedSection numSpaces imports, common)
  other -> (other, mempty)

-- |Strip matching dependencies from many nested sections.
stripNestedSections :: [T.NestedSection] -> Set T.DependencyName -> ([T.NestedSection], Set T.CommonName)
stripNestedSections nested dependencies = second mconcat $ unzip $ fmap (\x -> stripNestedSection x dependencies) nested

-- |Strip dependencies from any top-level section.
stripSection :: T.Section -> Set T.DependencyName -> StripTarget -> (T.Section, Set T.CommonName)
stripSection section dependencies target = case (section, target) of
  (T.TargetSection T.CompilableTypeLibrary Nothing nested, StripTargetBaseLibrary) ->
    let (newNested, common) = stripNestedSections nested dependencies
    in (T.TargetSection T.CompilableTypeLibrary Nothing newNested, common)
  (T.TargetSection typ (Just name) nested, StripTargetCompilable T.Compilable {..}) | typ == compilableType && name == compilableName ->
    let (newNested, common) = stripNestedSections nested dependencies
    in (T.TargetSection typ (Just name) newNested, common)
  (T.CommonSection name nested, StripTargetCommonStanza common) | Set.member name common ->
    let (newNested, newCommon) = stripNestedSections nested dependencies
    in (T.CommonSection name newNested, newCommon)
  (other, _) -> (other, mempty)

-- |Strip dependencies from many top-level sections.
stripSections :: [T.Section] -> Set T.DependencyName -> Maybe T.Compilable -> [T.Section]
stripSections sections dependencies compilableMay =
  let run target = second mconcat . unzip . fmap (\x -> stripSection x dependencies target)
      firstTarget = maybe StripTargetBaseLibrary StripTargetCompilable compilableMay
      firstPass = run firstTarget sections
  in flip fix firstPass $ \recur -> \case
       (final, none) | Set.null none -> final
       (next, common) -> recur (run (StripTargetCommonStanza common) next)
