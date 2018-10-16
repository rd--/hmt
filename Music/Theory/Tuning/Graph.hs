-- | Tuning graph related functions.
module Music.Theory.Tuning.Graph where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.Graph.Inductive.Graph as G {- fgl -}
import qualified Data.Graph.Inductive.PatriciaTree as G {- fgl -}

import qualified Music.Theory.Graph.Johnson_2014 as OH {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}
import qualified Music.Theory.Tuning.Scala as T {- hmt -}
import qualified Music.Theory.Tuning.Euler as T {- hmt -}

-- | R = Rational
type R = Rational

-- | V = Vertex (R)
type V = R

-- | E = Edge (V1,V2,V2-V1) where V1 < V2
type E = (R,R,R)

-- | G = Graph ([V],[E])
type G = ([V],[E])

-- | List of nodes at /g/ connected to node /r/.
g_edge_list :: G -> R -> [R]
g_edge_list (_,e) r =
  let f (p,q,_) = if r == p then Just q else if r == q then Just p else Nothing
  in mapMaybe f e

-- | Flip a ratio in (1,2) and multiply by 2.
--
-- > import Data.Ratio {- base -}
-- > map rflip [5%4,3%2,7%4] == [8%5,4%3,8%7]
-- > map rflip [3/2,5/4,7/4]
rflip :: R -> R
rflip n = if n < 1 || n > 2 then error "rflip" else 1 / n * 2

-- | nrm = normalise
rnrm :: R -> R
rnrm = T.ratio_interval_class_by id

rrel :: (R,R) -> R
rrel (p,q) = T.fold_ratio_to_octave_err (p / q)

iset_sym :: [R] -> [R]
iset_sym l = l ++ map rflip l

mk_graph :: [R] -> [R] -> G
mk_graph iset scl_r =
  (scl_r
  ,[(p,q,r) |
    p <- scl_r,
    q <- scl_r,
    p < q,
    let r = rnrm (rrel (p,q)),
    r `elem` iset_sym iset])

rem_oct :: [R] -> [R]
rem_oct r = if last r /= 2 then error "rem_oct" else T.drop_last r

mk_graph_scl :: [R] -> T.Scale Integer -> G
mk_graph_scl iset = mk_graph iset . rem_oct . T.scale_ratios_req

r_pcset_univ :: [R] -> [Int]
r_pcset_univ = nub . sort . map (T.ratio_to_pc 0)

-- | Does [R] construct indicated /pcset/.
r_is_pcset :: [Int] -> [R] -> Bool
r_is_pcset pcset = ((==) pcset) . sort . map (T.ratio_to_pc 0)

graph_to_fgl :: G -> G.Gr R R
graph_to_fgl (v,e) =
  let fgl_v = zip [0..] v
      r_to_v :: R -> Int
      r_to_v x = fromJust (T.reverse_lookup x fgl_v)
      fgl_e = map (\(p,q,i) -> (r_to_v p,r_to_v q,i)) e
  in G.mkGraph fgl_v fgl_e

mk_graph_fgl :: [R] -> [R] -> G.Gr R R
mk_graph_fgl iset r = graph_to_fgl (mk_graph iset r)

g_to_dot :: Int -> [(String,String)] -> (R -> [(String,String)]) -> G -> [String]
g_to_dot k attr v_attr (_,e_set) =
  OH.gen_graph
  (("edge:fontsize","9") : attr)
  (\v -> ("label",T.rat_label (k,True) v) : v_attr v
  ,\e -> [("label",T.rational_pp e)])
  (map (\(p,q,r) -> ((p,q),r)) e_set)

scl_to_dot :: ([R], Int, [(String, String)], R -> [(String, String)]) -> String -> IO [String]
scl_to_dot (iset,k,attr,v_attr) nm = do
  sc <- T.scl_load nm
  let gr = mk_graph_scl iset sc
  return (g_to_dot k attr v_attr gr)
