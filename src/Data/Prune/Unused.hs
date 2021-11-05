module Data.Prune.Unused where

import Prelude

import Data.Foldable (traverse_)
import Data.Monoid (Endo(Endo))
import Data.Set (Set)
import Data.Text (pack, unpack)
import qualified Data.Set as Set

import Data.Prune.Cabal (stripGenericPackageDescription)
import Data.Prune.Confirm (confirm)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import qualified Data.Prune.Confirm as Confirm
import qualified Data.Prune.Types as T

data Apply = NoApply | Apply | ApplyNoVerify
  deriving (Eq, Ord, Show)

validateApply :: Bool -> Bool -> Either String Apply
validateApply shouldApply noVerify = case (shouldApply, noVerify) of
  (True, True) -> pure ApplyNoVerify
  (True, False) -> pure Apply
  (False, True) -> Left "Must specify --apply-changes to use --no-verify"
  (False, False) -> pure NoApply

apply :: T.Package -> Set T.DependencyName -> Maybe T.Compilable -> Apply -> IO (Bool, Endo GenericPackageDescription)
apply T.Package {..} dependencies compilableMay = \case
  NoApply -> do
    printDependencies
    pure (True, mempty)
  Apply -> do
    printDependencies
    confirm "Apply these changes? (Y/n)" >>= \case
      False -> pure (False, mempty)
      True -> pure (False, Endo $ \x -> stripGenericPackageDescription x dependencies compilableMay)
  ApplyNoVerify -> do
    printDependencies
    pure (False, Endo $ \x -> stripGenericPackageDescription x dependencies compilableMay)
  where
    printDependencies = case compilableMay of
      Nothing -> do
        putStrLn . Confirm.warn . unpack $ "Some unused base dependencies for package " <> packageName
        traverse_ (putStrLn . unpack . ("  " <>) . T.unDependencyName) $ Set.toList dependencies
      Just T.Compilable {..} -> do
        putStrLn . Confirm.warn . unpack $ "Some unused dependencies for " <> pack (show compilableType) <> " " <> T.unCompilableName compilableName <> " in package " <> packageName
        traverse_ (putStrLn . unpack . ("  " <>) . T.unDependencyName) $ Set.toList dependencies
