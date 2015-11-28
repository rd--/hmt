-- | Directory functions.
module Music.Theory.Directory where

import Data.List {- base -}
import Data.Maybe {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}

-- | Scan a list of directories until a file is located, or not.
path_scan :: [FilePath] -> FilePath -> IO (Maybe FilePath)
path_scan p fn =
    case p of
      [] -> return Nothing
      dir:p' -> let nm = dir </> fn
                    f x = if x then return (Just nm) else path_scan p' fn
                in doesFileExist nm >>= f

path_scan_err :: [FilePath] -> FilePath -> IO FilePath
path_scan_err p x =
    let err = (error ("path_scan: " ++ show p ++ ": " ++ x))
    in fmap (fromMaybe err) (path_scan p x)

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

