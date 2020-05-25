{- | Graph/PLY functions.

This module is used instead of 'Music.Theory.Graph.OBJ' when edges are coloured.

There is no reader.

Greg Turk "The PLY Polygon File Format" (1994)

SEE "PLY_FILES.txt" in <https://www.cc.gatech.edu/projects/large_models/files/ply.tar.gz>

-}
module Music.Theory.Graph.PLY where

import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.Show as T {- hmt -}

-- | ASCII PLY-1.0 header for V3 graph of (#v,#e).
--   If n/e_clr is true nodes/edges are coloured.
ply_graph_header :: (Int,Int) -> [String]
ply_graph_header (n_v,n_e) =
  ["ply"
  ,"format ascii 1.0"
  ,"element vertex " ++ show n_v
  ,"property float x"
  ,"property float y"
  ,"property float z"
  ,"element edge " ++ show n_e
  ,"property int vertex1"
  ,"property int vertex2"
  ,"property uchar red"
  ,"property uchar green"
  ,"property uchar blue"
  ,"end_header"]

{- | Requires (but does not check) that graph vertices be indexed [0 .. #v - 1]
     Edges are coloured as U8 (red,green,blue) triples.
     It is an error (not checked) for there to be no edges.
-}
v3_graph_to_ply_clr :: Int -> T.LBL (Double,Double,Double) (Int,Int,Int) -> [String]
v3_graph_to_ply_clr k (v,e) =
  let v_pp (_,(x,y,z)) = unwords (map (T.double_pp k) [x,y,z])
      e_pp ((i,j),(r,g,b)) = unwords (map show [i,j,r,g,b])
  in concat [ply_graph_header (length v,length e)
            ,map v_pp v
            ,map e_pp e]
