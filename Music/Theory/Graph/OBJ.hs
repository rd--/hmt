-- | Graph/OBJ functions.
module Music.Theory.Graph.OBJ where

import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.Show as T {- hmt -}

{- | Requires graph vertices be indexed [0 .. #v - 1]
OBJ file vertices are one-indexed.
If /wr_p/ is True point entries are written.
-}
v3_graph_to_obj :: Bool -> Maybe Int -> T.LBL (Double,Double,Double) () -> [String]
v3_graph_to_obj wr_p k (v,e) =
  let v_pp (_,(x,y,z)) = unwords ("v" : map (maybe show T.double_pp k) [x,y,z])
      e_pp ((i,j),()) = unwords ("l" : map show [i + 1,j + 1])
  in concat [map v_pp v
            ,if wr_p then map (\i -> "p " ++ show i) [1 .. length v] else []
            ,map e_pp e]
