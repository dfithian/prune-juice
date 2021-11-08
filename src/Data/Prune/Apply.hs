{-# LANGUAGE TypeFamilyDependencies #-}
module Data.Prune.Apply where

import Prelude

import Data.Foldable (traverse_)
import Data.Monoid (Endo(Endo), appEndo)
import Data.Set (Set)
import Data.Text (pack, unpack)
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import qualified Data.Set as Set

import Data.Prune.ApplyStrategy.Safe (stripGenericPackageDescription)
import Data.Prune.ApplyStrategy.Smart (stripSections)
import Data.Prune.Confirm (confirm)
import Data.Prune.Section.Parser (writeCabalSections)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import qualified Data.Prune.Confirm as Confirm
import qualified Data.Prune.Section.Types as T
import qualified Data.Prune.Types as T

data Apply (a :: T.ApplyStrategy) where
  ApplySafe :: FilePath -> GenericPackageDescription -> Endo GenericPackageDescription -> Apply 'T.ApplyStrategySafe
  ApplySmart :: FilePath -> [T.Section] -> Endo [T.Section] -> Apply 'T.ApplyStrategySmart

data SomeApply = forall (a :: T.ApplyStrategy). SomeApply { unSomeApply :: Apply a }

runApply :: SomeApply -> T.Package -> Set T.DependencyName -> Maybe T.Compilable -> T.ShouldApply -> IO (Bool, SomeApply)
runApply (SomeApply ap) T.Package {..} dependencies compilableMay = \case 
  T.ShouldNotApply -> do
    printDependencies
    pure (True, applyNoop)
  T.ShouldApply -> do
    printDependencies
    confirm "Apply these changes? (Y/n)" >>= \case
      False -> pure (False, applyNoop)
      True -> pure (False, applyOnce)
  T.ShouldApplyNoVerify -> do
    printDependencies
    pure (False, applyOnce)
  where
    printDependencies = case compilableMay of
      Nothing -> do
        putStrLn . Confirm.warn . unpack $ "Some unused base dependencies for package " <> packageName
        traverse_ (putStrLn . unpack . ("  " <>) . T.unDependencyName) $ Set.toList dependencies
      Just T.Compilable {..} -> do
        putStrLn . Confirm.warn . unpack $ "Some unused dependencies for " <> pack (show compilableType) <> " " <> T.unCompilableName compilableName <> " in package " <> packageName
        traverse_ (putStrLn . unpack . ("  " <>) . T.unDependencyName) $ Set.toList dependencies
    applyNoop = case ap of
      ApplySafe x y z -> SomeApply $ ApplySafe x y z
      ApplySmart x y z -> SomeApply $ ApplySmart x y z
    applyOnce = case ap of
      ApplySafe x y z -> SomeApply $ ApplySafe x y $ z <> Endo (\w -> stripGenericPackageDescription w dependencies compilableMay)
      ApplySmart x y z -> SomeApply $ ApplySmart x y $ z <> Endo (\w -> stripSections w dependencies compilableMay)

writeApply :: SomeApply -> IO ()
writeApply (SomeApply ap) = case ap of
  ApplySafe fp description endo -> writeGenericPackageDescription fp (appEndo endo description)
  ApplySmart fp parsed endo -> writeCabalSections fp (appEndo endo parsed)
