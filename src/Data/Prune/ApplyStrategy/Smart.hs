module Data.Prune.ApplyStrategy.Smart where

import Prelude

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Text (pack, splitOn, unpack)
import Text.Regex (Regex, matchRegex, mkRegex)
import qualified Data.Set as Set

import qualified Data.Prune.Section.Types as T
import qualified Data.Prune.Types as T

data TokenType
  = TokenTypeSpace
  | TokenTypeComma
  | TokenTypeDependency
  | TokenTypeVersionRange
  deriving (Eq, Ord, Show)

dependencyNameRegex :: Regex
dependencyNameRegex = mkRegex "^ *([a-zA-Z0-9]+).*$"

matchDependencyName :: String -> Maybe T.DependencyName
matchDependencyName str = Just . T.DependencyName . pack =<< T.headMay =<< matchRegex dependencyNameRegex str

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

stripBuildDepends :: [String] -> Set T.DependencyName -> [String]
stripBuildDepends buildDepends dependencies = mapMaybe (\x -> stripOneBuildDepends x dependencies) buildDepends

stripNestedSection :: T.NestedSection -> Set T.DependencyName -> T.NestedSection
stripNestedSection nested dependencies = case nested of
  T.BuildDependsNestedSection numSpaces buildDepends -> T.BuildDependsNestedSection numSpaces (stripBuildDepends buildDepends dependencies)
  other -> other

stripNestedSections :: [T.NestedSection] -> Set T.DependencyName -> [T.NestedSection]
stripNestedSections nested dependencies = fmap (\x -> stripNestedSection x dependencies) nested

stripSection :: T.Section -> Set T.DependencyName -> Maybe T.Compilable -> T.Section
stripSection section dependencies compilableMay = case (section, compilableMay) of
  (T.TargetSection T.CompilableTypeLibrary Nothing nested, Nothing) ->
    T.TargetSection T.CompilableTypeLibrary Nothing (stripNestedSections nested dependencies)
  (T.TargetSection typ (Just name) nested, Just T.Compilable {..}) | typ == compilableType && name == compilableName ->
    T.TargetSection typ (Just name) (stripNestedSections nested dependencies)
  (other, _) -> other

stripSections :: [T.Section] -> Set T.DependencyName -> Maybe T.Compilable -> [T.Section]
stripSections sections dependencies compilableMay = fmap (\x -> stripSection x dependencies compilableMay) sections
