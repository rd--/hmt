-- | Directory functions.
module Music.Theory.Directory where

import Data.List {- base -}
import Data.Maybe {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}
import System.Process {- process -}

-- | Scan a list of directories until a file is located, or not.
--   This does not traverse any sub-directory structure.
path_scan :: [FilePath] -> FilePath -> IO (Maybe FilePath)
path_scan p fn =
    case p of
      [] -> return Nothing
      dir:p' -> let nm = dir </> fn
                    f x = if x then return (Just nm) else path_scan p' fn
                in doesFileExist nm >>= f

-- | Erroring variant.
path_scan_err :: [FilePath] -> FilePath -> IO FilePath
path_scan_err p x =
    let err = (error ("path_scan: " ++ show p ++ ": " ++ x))
    in fmap (fromMaybe err) (path_scan p x)

-- | Find files having case-insensitive filename extension.
--   This runs the system utility /find/, so is UNIX only.
--
-- > dir_find_ext ".syx" "/home/rohan/sw/hsc3-data/data/yamaha/"
dir_find_ext :: String -> FilePath -> IO [FilePath]
dir_find_ext ext dir = fmap lines (readProcess "find" [dir,"-iname",'*' : ext] "")

-- | Subset of files in /dir/ with an extension in /ext/.
--   Extensions include the leading dot and are case-sensitive.
--
-- > dir_subset [".hs"] "/home/rohan/sw/hmt/cmd"
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

