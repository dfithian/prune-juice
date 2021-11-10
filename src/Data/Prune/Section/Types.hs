-- |Description: AST for the "Data.Prune.ApplyStrategy.Smart" strategy.
module Data.Prune.Section.Types where

import Prelude

import Data.Text (Text)

import qualified Data.Prune.Types as T

-- |The name of a @common@ stanza.
newtype CommonName = CommonName { unCommonName :: Text }
  deriving (Eq, Ord, Show)

-- |An indented section.
data NestedSection
  = BuildDependsNestedSection Int [String]
  | ImportNestedSection Int [String]
  | OtherNestedSection Int [String]
  deriving (Eq, Ord, Show)

-- |A top-level section.
data Section
  = TargetSection T.CompilableType (Maybe T.CompilableName) [NestedSection]
  | CommonSection CommonName [NestedSection]
  | OtherSection [String]
  deriving (Eq, Ord, Show)
