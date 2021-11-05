module Data.Prune.Unused where

import Prelude

import Data.Foldable (traverse_)
import Data.Set (Set)
import Data.Text (pack, unpack)
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import qualified Data.Set as Set

import Data.Prune.Cabal (stripGenericPackageDescription)
import Data.Prune.Confirm (confirm)
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

apply :: T.Package -> Set T.DependencyName -> Maybe T.Compilable -> Apply -> IO Bool
apply T.Package {..} dependencies compilableMay = \case
  NoApply -> do
    printDependencies
    pure True
  Apply -> do
    printDependencies
    T.whenM (confirm "Apply these changes? (Y/n)") run
    pure False
  ApplyNoVerify -> do
    printDependencies
    putStrLn $ Confirm.bold "Applying..."
    run
    pure False
  where
    printDependencies = case compilableMay of
      Nothing -> do
        putStrLn . Confirm.warn . unpack $ "Some unused base dependencies for package " <> packageName
        traverse_ (putStrLn . unpack . ("  " <>) . T.unDependencyName) $ Set.toList dependencies
      Just T.Compilable {..} -> do
        putStrLn . Confirm.warn . unpack $ "Some unused dependencies for " <> pack (show compilableType) <> " " <> T.unCompilableName compilableName <> " in package " <> packageName
        traverse_ (putStrLn . unpack . ("  " <>) . T.unDependencyName) $ Set.toList dependencies
    run = writeGenericPackageDescription packageFile $
      stripGenericPackageDescription packageDescription dependencies compilableMay
      
