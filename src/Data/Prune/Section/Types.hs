module Data.Prune.Section.Types where

import Prelude

import Data.Text (Text)

import qualified Data.Prune.Types as T

newtype CommonName = CommonName { unCommonName :: Text }
  deriving (Eq, Ord, Show)

data NestedSection
  = BuildDependsNestedSection Int [String]
  | ImportNestedSection Int [String]
  | OtherNestedSection Int [String]
  deriving (Eq, Ord, Show)

data Section
  = TargetSection T.CompilableType (Maybe T.CompilableName) [NestedSection]
  | CommonSection CommonName [NestedSection]
  | OtherSection [String]
  deriving (Eq, Ord, Show)
