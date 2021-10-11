module Data.Prune.File (listFilesRecursive) where

import Prelude

import Data.Set (Set)
import Data.Traversable (for)
import System.Directory (doesDirectoryExist, listDirectory, pathIsSymbolicLink)
import System.FilePath.Posix ((</>))
import qualified Data.Set as Set

-- |Recursively list files in a directory, ignoring symlinks.
listFilesRecursive :: FilePath -> IO (Set FilePath)
listFilesRecursive dir = do
  dirs <- listDirectory dir
  fmap mconcat . for dirs $ \case
    -- don't include "hidden" directories, i.e. those that start with a '.'
    '.' : _ -> pure mempty
    fn -> do
      let
        path = if dir == "." then fn else dir </> fn
      isDir <- doesDirectoryExist path
      isSymlink <- pathIsSymbolicLink path
      case (isSymlink, isDir) of
        (True, True) -> pure mempty
        (_, True) -> listFilesRecursive path
        _ -> pure $ Set.singleton path
