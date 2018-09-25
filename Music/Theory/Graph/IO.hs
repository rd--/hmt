module Music.Theory.Graph.IO where

import Data.List.Split {- split -}
import System.Process {- process -}

import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

-- * G6

-- | Load Graph6 file, discard optional header if present.
g6_load :: FilePath -> IO [String]
g6_load fn = do
  s <- readFile fn
  let s' = if take 6 s == ">>graph6<<" then drop 6 s else s
  return (lines s')

-- | Load G6 file variant where each line is "Description\tG6"
g6_dsc_load :: FilePath -> IO [(String,String)]
g6_dsc_load fn = do
  s <- readFile fn
  let r = map (T.split_on_1_err "\t") (lines s)
  return r

-- | Call nauty-listg to transform a sequence of G6.
g6_to_el :: [String] -> IO [T.EL]
g6_to_el g6 = do
  r <- readProcess "nauty-listg" ["-q","-l0","-e"] (unlines g6)
  return (map T.el_parse (chunksOf 2 (lines r)))

-- | 'T.el_to_gr' of 'g6_to_el'
g6_to_gr :: [String] -> IO [T.GR]
g6_to_gr = fmap (map T.el_to_gr) . g6_to_el

-- | 'g6_to_gr' of 'g6_dsc_load'
g6_dsc_load_gr :: FilePath -> IO [(String,T.GR)]
g6_dsc_load_gr fn = do
  dat <- g6_dsc_load fn
  let (dsc,g6) = unzip dat
  gr <- g6_to_gr g6
  return (zip dsc gr)

