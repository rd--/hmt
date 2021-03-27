{- | graph6 graph encoding

<http://users.cecs.anu.edu.au/~bdm/nauty/>
<https://users.cecs.anu.edu.au/~bdm/data/formats.html>
-}
module Music.Theory.Graph.G6 where

import Data.Bifunctor {- base -}

import qualified Data.List.Split as Split {- split -}
import qualified System.Process as Process {- process -}

import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

-- * G6 (graph6)

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

-- | Call nauty-listg to transform a sequence of G6. (debian = nauty)
g6_to_edg :: [String] -> IO [T.EDG]
g6_to_edg g6 = do
  r <- Process.readProcess "nauty-listg" ["-q","-l0","-e"] (unlines g6)
  return (map T.edg_parse (Split.chunksOf 2 (lines r)))

-- | 'T.edg_to_g' of 'g6_to_edg'
g6_to_g :: [String] -> IO [T.G]
g6_to_g = fmap (map T.edg_to_g) . g6_to_edg

-- | 'g6_to_edg' of 'g6_dsc_load'.
g6_dsc_load_edg :: FilePath -> IO [(String,T.EDG)]
g6_dsc_load_edg fn = do
  dat <- g6_dsc_load fn
  let (dsc,g6) = unzip dat
  gr <- g6_to_edg g6
  return (zip dsc gr)

-- | 'T.edg_to_g' of 'g6_dsc_load_edg'
g6_dsc_load_gr :: FilePath -> IO [(String,T.G)]
g6_dsc_load_gr = fmap (map (second T.edg_to_g)) . g6_dsc_load_edg

{- | Generate the text format read by nauty-amtog.

> e = ((4,3),[(0,3),(1,3),(2,3)])
> m = T.edg_to_adj_mtx_undir (0,1) e
> putStrLn (adj_mtx_to_am m)

-}
adj_mtx_to_am :: T.ADJ_MTX Int -> String
adj_mtx_to_am (nv,mtx) =
  unlines ["n=" ++ show nv
          ,"m"
          ,unlines (map (unwords . map show) mtx)]

-- | Call nauty-amtog to transform a sequence of ADJ_MTX to G6.
--
-- > adj_mtx_to_g6 [m,m]
adj_mtx_to_g6 :: [T.ADJ_MTX Int] -> IO [String]
adj_mtx_to_g6 adj = do
  r <- Process.readProcess "nauty-amtog" ["-q"] (unlines (map adj_mtx_to_am adj))
  return (lines r)

-- | 'adj_mtx_to_g6' of 'T.g_to_adj_mtx_undir'
g_to_g6 :: [T.G] -> IO [String]
g_to_g6 = adj_mtx_to_g6 . map (T.g_to_adj_mtx_undir (0,1))

-- | 'writeFile' of 'g_to_g6'
g_store_g6 :: FilePath -> [T.G] -> IO ()
g_store_g6 fn gr = g_to_g6 gr >>= writeFile fn . unlines

-- | Call nauty-labelg to canonise a set of graphs.
g6_labelg :: [String] -> IO [String]
g6_labelg = fmap lines . Process.readProcess "nauty-labelg" ["-q"] . unlines

{- | 'g6_to_g' of 'g6_labelg' of 'g_to_g6'

> g1 = ([0,1,2,3],[(0,3),(3,1),(3,2),(1,2)])
> g2 = ([0,1,2,3],[(1,0),(0,3),(0,2),(2,3)])
> [g3,g4] <- g_labelg [g1,g2]
> (g1 == g2,g3 == g4)
-}
g_labelg :: [T.G] -> IO [T.G]
g_labelg g = g_to_g6 g >>= g6_labelg >>= g6_to_g
