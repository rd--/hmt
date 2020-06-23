{- | Graph/OBJ functions

This module is primarily for reading & writing graphs where vertices are labeled (x,y,z) to OBJ files.

PDF=<http://www.cs.utah.edu/~boulos/cs3505/obj_spec.pdf>
TXT=<http://www.martinreddy.net/gfx/3d/OBJ.spec>
-}
module Music.Theory.Graph.OBJ where

import Data.Either {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Show as T {- hmt -}

{- | Requires (but does not check) that graph vertices be indexed [0 .. #v - 1]
OBJ file vertices are one-indexed.
If /wr_p/ is True point (p) entries are written.
-}
v3_graph_to_obj_opt :: RealFloat n => Bool -> Int -> T.LBL (n,n,n) () -> [String]
v3_graph_to_obj_opt wr_p k (v,e) =
  let v_pp (_,(x,y,z)) = unwords ("v" : map (T.realfloat_pp k) [x,y,z])
      e_pp ((i,j),()) = unwords ("l" : map show [i + 1,j + 1])
  in concat [map v_pp v
            ,if wr_p then map (\i -> "p " ++ show i) [1 .. length v] else []
            ,map e_pp e]

-- | 'v3_graph_to_obj_opt' 'False'.
v3_graph_to_obj :: RealFloat n => Int -> T.LBL (n,n,n) () -> [String]
v3_graph_to_obj = v3_graph_to_obj_opt False

-- | 'writeFile' of 'v3_graph_to_obj'.
obj_store_v3_graph :: RealFloat n => Int -> FilePath -> (T.LBL (n,n,n) ()) -> IO ()
obj_store_v3_graph k fn = writeFile fn . unlines . v3_graph_to_obj k

-- | Empty lines are allowed and ignored, comments are #-prefixed.
obj_is_nil_line :: String -> Bool
obj_is_nil_line s = null s || head s == '#'

-- | Read OBJ file consisting only of /v/, /l/ and /f/ (and optionally /p/, which are ignored) entries.
obj_to_v3_graph :: Read n => [String] -> T.LBL (n,n,n) ()
obj_to_v3_graph txt =
  let l_verify (i,j) = if i < 0 || j < 0 then error "obj_to_v3_graph?" else (i,j)
      e_read (i,j) = l_verify (read i - 1,read j - 1)
      f s = case words s of
              ["v",x,y,z] -> Just (Left (read x,read y,read z))
              "l":ix -> Just (Right (map e_read (T.adj2 1 ix)))
              "f":ix -> Just (Right (map e_read (T.adj2_cyclic 1 ix)))
              ["p",_] -> Nothing
              _ -> error "obj_to_v3_graph?"
      (v,l) = partitionEithers (mapMaybe f txt)
  in (zip [0..] v,zip (concat l) (repeat ()))

-- | 'obj_to_v3_graph' of 'readFile'.
obj_load_v3_graph :: Read n => FilePath -> IO (T.LBL (n,n,n) ())
obj_load_v3_graph = fmap (obj_to_v3_graph . filter (not . obj_is_nil_line) . lines) . readFile

-- * F64

-- | Type-specialised.
v3_graph_to_obj_f64 :: Int -> T.LBL (Double,Double,Double) () -> [String]
v3_graph_to_obj_f64 = v3_graph_to_obj

-- | Type-specialised.
obj_store_v3_graph_f64 :: Int -> FilePath -> (T.LBL (Double,Double,Double) ()) -> IO ()
obj_store_v3_graph_f64 = obj_store_v3_graph

-- | Type-specialised.
obj_load_v3_graph_f64 :: FilePath -> IO (T.LBL (Double,Double,Double) ())
obj_load_v3_graph_f64 = obj_load_v3_graph

-- * FACES

-- | Rewrite a set of faces (CCW triples of (x,y,z) coordinates) as (vertices,[[v-indices]]).
--   Vertices are zero-indexed.
obj_face_set_dat :: Ord n => [[(n,n,n)]] -> ([(n,n,n)],[[Int]])
obj_face_set_dat t =
  let v = nub (sort (concat t))
      v_ix = zip [0..] v
      f = map (map (flip T.reverse_lookup_err v_ix)) t
  in (v,f)

-- | Inverse of 'obj_face_set_dat'.
obj_face_dat_set :: ([(n,n,n)],[[Int]]) -> [[(n,n,n)]]
obj_face_dat_set (v,f) = map (map (flip T.lookup_err (zip [0..] v))) f

obj_face_dat_fmt :: (Show n, Ord n) => ([(n,n,n)],[[Int]]) -> [String]
obj_face_dat_fmt (v,f) =
  let v_f (x,y,z) = unwords ["v",show x,show y,show z]
      f_f = unwords . ("f" :) . map show . map (+ 1)
  in map v_f v ++ map f_f f

obj_face_dat_store :: (Show n, Ord n) => FilePath -> ([(n,n,n)],[[Int]]) -> IO ()
obj_face_dat_store fn = writeFile fn . unlines . obj_face_dat_fmt

-- | Format 'obj_face_set_dat' as an OBJ file. OBJ files are one-indexed.
obj_face_set_fmt :: (Show n, Ord n) => [[(n,n,n)]] -> [String]
obj_face_set_fmt = obj_face_dat_fmt . obj_face_set_dat

-- | 'writeFile' of 'obj_face_set_fmt'
obj_face_set_store :: (Show n, Ord n) => FilePath -> [[(n,n,n)]] -> IO ()
obj_face_set_store fn = writeFile fn . unlines . obj_face_set_fmt
