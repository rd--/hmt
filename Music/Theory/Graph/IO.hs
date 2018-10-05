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
g6_to_edg :: [String] -> IO [T.EDG]
g6_to_edg g6 = do
  r <- readProcess "nauty-listg" ["-q","-l0","-e"] (unlines g6)
  return (map T.edg_parse (chunksOf 2 (lines r)))

-- | 'T.edg_to_g' of 'g6_to_edg'
g6_to_gr :: [String] -> IO [T.G]
g6_to_gr = fmap (map T.edg_to_g) . g6_to_edg

g6_dsc_load_edg :: FilePath -> IO [(String,T.EDG)]
g6_dsc_load_edg fn = do
  dat <- g6_dsc_load fn
  let (dsc,g6) = unzip dat
  gr <- g6_to_edg g6
  return (zip dsc gr)

-- | 'T.edg_to_g' of 'g6_dsc_load_edg'
g6_dsc_load_gr :: FilePath -> IO [(String,T.G)]
g6_dsc_load_gr = fmap (map (\(dsc,e) -> (dsc,T.edg_to_g e))) . g6_dsc_load_edg

{- | Generate the text format read by nauty-amtog.

> e = ((4,3),[(0,3),(1,3),(2,3)])
> m = T.edg_to_adj_mtx_undir e
> putStrLn (adj_mtx_to_am m)

-}
adj_mtx_to_am :: T.ADJ_MTX -> String
adj_mtx_to_am (nv,mtx) =
  unlines ["n=" ++ show nv
          ,"m"
          ,unlines (map (unwords . map show) mtx)]

-- | Call nauty-amtog to transform a sequence of ADJ_MTX to G6.
--
-- > adj_mtx_to_g6 [m,m]
adj_mtx_to_g6 :: [T.ADJ_MTX] -> IO [String]
adj_mtx_to_g6 adj = do
  r <- readProcess "nauty-amtog" ["-q"] (unlines (map adj_mtx_to_am adj))
  return (lines r)
