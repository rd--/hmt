{- | LGL = Large Graph Layout (NCOL, LGL)

<http://lgl.sourceforge.net/#FileFormat>
-}
module Music.Theory.Graph.LGL where

import Data.Bifunctor {- base -}
import Data.List {- base -}

import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.Show as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

-- * NCOL

-- | (edge,weight)
type NCOL_ENT t = ((t,t),Maybe Double)

-- | [ncol-entry]
type NCOL t = [NCOL_ENT t]

-- | Parse 'NCOL_ENT' from 'String'
ncol_parse :: Read t => String -> NCOL_ENT t
ncol_parse s =
  case words s of
    [i,j] -> ((read i,read j),Nothing)
    [i,j,k] -> ((read i,read j),read k)
    _ -> error "ncol_parse"

-- | Load 'NCOL' from .ncol file.
ncol_load :: Read t => FilePath -> IO (NCOL t)
ncol_load = fmap (map ncol_parse . lines) . readFile

-- | Type-specialised.
ncol_load_int :: FilePath -> IO (NCOL Int)
ncol_load_int = ncol_load

{- | Format NCOL_ENT.

> ncol_ent_format 4 ((0,1),Nothing) == "0 1"
> ncol_ent_format 4 ((0,1),Just 2.0) == "0 1 2.0000"
-}
ncol_ent_format :: Show t => Int -> NCOL_ENT t -> String
ncol_ent_format k ((i,j),w) = unwords (map show [i,j]) ++ maybe "" ((' ':) . T.double_pp k) w

-- | Store 'NCOL' of 'Int' to .ncol file
ncol_store :: Show t => Int -> FilePath -> NCOL t -> IO ()
ncol_store k fn dat = writeFile fn (unlines (map (ncol_ent_format k) dat))

-- | Type-specialised.
ncol_store_int :: Int -> FilePath -> NCOL Int -> IO ()
ncol_store_int = ncol_store

-- | NCOL data must be un-directed and have no self-arcs.
--   This function sorts edges (i,j) so that i <= j and deletes edges where i == j.
ncol_rewrite_eset :: Ord t => [(t,t)] -> [(t,t)]
ncol_rewrite_eset e = filter (uncurry (/=)) (nub (sort (map T.t2_sort e)))

-- | eset (edge-set) to NCOL (runs 'ncol_rewrite_eset')
eset_to_ncol :: Ord t => [(t,t)] -> NCOL t
eset_to_ncol = map (\e -> (e,Nothing)) . ncol_rewrite_eset

-- | Inverse of 'eset_to_ncol', 'error' if 'NCOL' is weighted
ncol_to_eset :: NCOL t -> [(t,t)]
ncol_to_eset = map (\(e,w) -> case w of {Nothing -> e;_ -> error "ncol_to_eset?"})

-- | 'ncol_store' of 'eset_to_ncol'
ncol_store_eset :: (Ord t,Show t) => FilePath -> [(t,t)] -> IO ()
ncol_store_eset fn = ncol_store undefined fn . eset_to_ncol

-- * LGL

-- | LGL is an adjaceny set with optional weights.
type LGL t = [(t,[(t,Maybe Double)])]

-- | Format 'LGL', k is floating point precision for optional weights.
lgl_format :: Show t => Int -> LGL t -> String
lgl_format k =
  let f (i,j) = show i ++ maybe "" ((' ' :) . T.double_pp k) j
      g (i,j) = unlines (('#' : ' ' : show i) : map f j)
  in concatMap g

-- | 'writeFile' of 'lgl_format'
lgl_store :: Show t => Int -> FilePath -> LGL t -> IO ()
lgl_store k fn = writeFile fn . lgl_format k

-- | adj (adjaceny-set) to 'LGL'.
adj_to_lgl :: T.ADJ t -> LGL t
adj_to_lgl = map (\(i,j) -> (i,zip j (repeat Nothing)))

-- | Inverse of 'adj_to_lgl', 'error' if 'LGL' is weighted
lgl_to_adj :: LGL t -> T.ADJ t
lgl_to_adj = map (second (map (\(k,w) -> case w of {Nothing -> k;_ -> error "lgl_to_adj?"})))

-- | 'lgl_store' of 'adj_to_lgl'
lgl_store_adj :: Show t => FilePath -> T.ADJ t -> IO ()
lgl_store_adj fn = lgl_store undefined fn . adj_to_lgl

-- > putStrLn $ lgl_format 4 $ adj_to_lgl [(0,[1,2,3]),(1,[2,3]),(2,[3])]
