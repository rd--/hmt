{- | Graph/OBJ functions

PDF=<http://www.cs.utah.edu/~boulos/cs3505/obj_spec.pdf>
TXT=<http://www.martinreddy.net/gfx/3d/OBJ.spec>
-}
module Music.Theory.Graph.OBJ where

import Data.Either {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.Show as T {- hmt -}

{- | Requires graph vertices be indexed [0 .. #v - 1]
OBJ file vertices are one-indexed.
If /wr_p/ is True point entries are written.
-}
v3_graph_to_obj_opt :: Bool -> Maybe Int -> T.LBL (Double,Double,Double) () -> [String]
v3_graph_to_obj_opt wr_p k (v,e) =
  let v_pp (_,(x,y,z)) = unwords ("v" : map (maybe show T.double_pp k) [x,y,z])
      e_pp ((i,j),()) = unwords ("l" : map show [i + 1,j + 1])
  in concat [map v_pp v
            ,if wr_p then map (\i -> "p " ++ show i) [1 .. length v] else []
            ,map e_pp e]

v3_graph_to_obj :: Maybe Int -> T.LBL (Double, Double, Double) () -> [String]
v3_graph_to_obj = v3_graph_to_obj_opt False

-- | Read OBJ file consisting only of /v/ and /l/ (and optionally /p/) entries.
obj_to_v3_graph :: [String] -> T.LBL (Double,Double,Double) ()
obj_to_v3_graph txt =
  let l_verify (i,j) = if i < 0 || j < 0 then error "obj_to_v3_graph?" else (i,j)
      f s = case words s of
              ["v",x,y,z] -> Just (Left (read x,read y,read z))
              ["l",i,j] -> Just (Right (l_verify (read i - 1,read j - 1)))
              ["p",_] -> Nothing
              _ -> error "obj_to_v3_graph?"
      (v,l) = partitionEithers (mapMaybe f txt)
  in (zip [0..] v,zip l (repeat ()))

obj_load_v3_graph :: FilePath -> IO (T.LBL (Double, Double, Double) ())
obj_load_v3_graph = fmap (obj_to_v3_graph . lines) . readFile
