-- | Directory functions.
module Music.Theory.Directory where

import Data.List {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

-- | Subset of files in /dir/ with an extension in /ext/.
dir_subset :: [String] -> FilePath -> IO [FilePath]
dir_subset ext dir = do
  let f nm = takeExtension nm `elem` ext
  c <- getDirectoryContents dir
  return (map (dir </>) (sort (filter f c)))
