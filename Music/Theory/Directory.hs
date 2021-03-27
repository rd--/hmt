-- | Directory functions.
module Music.Theory.Directory where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.Directory {- directory -}
import System.FilePath {- filepath -}
import System.Process {- process -}

import qualified Music.Theory.Monad as T {- hmt -}

{- | 'takeDirectory' gives different answers depending on whether there is a trailing separator.

> x = ["x/y","x/y/","x","/"]
> map parent_dir x == ["x","x",".","/"]
> map takeDirectory x == ["x","x/y",".","/"]
-}
parent_dir :: FilePath -> FilePath
parent_dir = takeDirectory . dropTrailingPathSeparator

-- | Scan a list of directories until a file is located, or not.
--   This does not traverse any sub-directory structure.
--
-- > mapM (path_scan ["/sbin","/usr/bin"]) ["fsck","ghc"]
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
    let err = error (concat ["path_scan: ",show p,": ",x])
    in fmap (fromMaybe err) (path_scan p x)

-- | Get sorted list of files at /dir/ with /ext/, ie. ls dir/*.ext
--
-- > dir_list_ext "/home/rohan/rd/j/" ".hs"
dir_list_ext :: FilePath -> String -> IO [FilePath]
dir_list_ext dir ext = do
  l <- listDirectory dir
  let fn = filter ((==) ext . takeExtension) l
  return (sort fn)

-- | Post-process 'dir_list_ext' to gives file-names with /dir/ prefix.
--
-- > dir_list_ext_path "/home/rohan/rd/j/" ".hs"
dir_list_ext_path :: FilePath -> String -> IO [FilePath]
dir_list_ext_path dir ext = fmap (map (dir </>)) (dir_list_ext dir ext)

-- | Find files having indicated filename.
--   This runs the system utility /find/, so is UNIX only.
--
-- > dir_find "DX7-ROM1A.syx" "/home/rohan/sw/hsc3-data/data/yamaha/"
dir_find :: FilePath -> FilePath -> IO [FilePath]
dir_find fn dir = fmap lines (readProcess "find" [dir,"-name",fn] "")

-- | Require that exactly one file is located, else error.
--
-- > dir_find_1 "DX7-ROM1A.syx" "/home/rohan/sw/hsc3-data/data/yamaha/"
dir_find_1 :: FilePath -> FilePath -> IO FilePath
dir_find_1 fn dir = do
  r <- dir_find fn dir
  case r of
    [x] -> return x
    _ -> error "dir_find_1?"

-- | Recursively find files having case-insensitive filename extension.
--   This runs the system utility /find/, so is UNIX only.
--
-- > dir_find_ext ".syx" "/home/rohan/sw/hsc3-data/data/yamaha/"
dir_find_ext :: String -> FilePath -> IO [FilePath]
dir_find_ext ext dir = fmap lines (readProcess "find" [dir,"-iname",'*' : ext] "")

-- | Post-process 'dir_find_ext' to delete starting directory.
--
-- > dir_find_ext_rel ".syx" "/home/rohan/sw/hsc3-data/data/yamaha/"
dir_find_ext_rel :: String -> FilePath -> IO [FilePath]
dir_find_ext_rel ext dir =
  let f = fromMaybe (error "dir_find_ext_rel?") . stripPrefix dir
  in fmap (map f) (dir_find_ext ext dir)

-- | Subset of files in /dir/ with an extension in /ext/.
--   Extensions include the leading dot and are case-sensitive.
--   Results are relative to /dir/.
dir_subset_rel :: [String] -> FilePath -> IO [FilePath]
dir_subset_rel ext dir = do
  let f nm = takeExtension nm `elem` ext
  c <- getDirectoryContents dir
  return (sort (filter f c))

-- | Variant where results have dir/ prefix.
--
-- > dir_subset [".hs"] "/home/rohan/sw/hmt/cmd"
dir_subset :: [String] -> FilePath -> IO [FilePath]
dir_subset ext dir = fmap (map (dir </>)) (dir_subset_rel ext dir)

-- | Subdirectories (relative) of /dir/.
dir_subdirs_rel :: FilePath -> IO [FilePath]
dir_subdirs_rel dir =
  let sel fn = doesDirectoryExist (dir </> fn)
  in listDirectory dir >>= filterM sel

-- | Subdirectories of /dir/.
dir_subdirs :: FilePath -> IO [FilePath]
dir_subdirs dir = fmap (map (dir </>)) (dir_subdirs_rel dir)

-- | If path is not absolute, prepend current working directory.
--
-- > to_absolute_cwd "x"
to_absolute_cwd :: FilePath -> IO FilePath
to_absolute_cwd x =
    if isAbsolute x
    then return x
    else fmap (</> x) getCurrentDirectory

-- | If /i/ is an existing file then /j/ else /k/.
if_file_exists :: (FilePath,IO t,IO t) -> IO t
if_file_exists (i,j,k) = T.m_if (doesFileExist i,j,k)

-- | 'createDirectoryIfMissing' (including parents) and then 'writeFile'
writeFile_mkdir :: FilePath -> String -> IO ()
writeFile_mkdir fn s = do
  let dir = takeDirectory fn
  createDirectoryIfMissing True dir
  writeFile fn s

-- | 'writeFile_mkdir' only if file does not exist.
writeFile_mkdir_x :: FilePath -> String -> IO ()
writeFile_mkdir_x fn txt = if_file_exists (fn,return (),writeFile_mkdir fn txt)
