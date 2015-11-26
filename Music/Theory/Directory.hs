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

-- | If path is not absolute, prepend current working directory.
--
-- > to_absolute_cwd "x"
to_absolute_cwd :: FilePath -> IO FilePath
to_absolute_cwd x =
    if isAbsolute x
    then return x
    else fmap (</> x) getCurrentDirectory

