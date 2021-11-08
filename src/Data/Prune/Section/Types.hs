module Data.Prune.Section.Types where

import Prelude

import qualified Data.Prune.Types as T

data NestedSection
  = BuildDependsNestedSection Int [String]
  | OtherNestedSection Int [String]
  deriving (Eq, Ord, Show)

data Section
  = TargetSection T.CompilableType (Maybe T.CompilableName) [NestedSection]
  | OtherSection [String]
  deriving (Eq, Ord, Show)
