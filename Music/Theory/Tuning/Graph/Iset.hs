-- | Tuning graph with edges determined by interval set.
module Music.Theory.Tuning.Graph.Iset where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.Graph.Inductive.Graph as Fgl {- fgl -}
import qualified Data.Graph.Inductive.PatriciaTree as Fgl {- fgl -}

import qualified Music.Theory.Graph.Dot as Dot {- hmt-base -}
import qualified Music.Theory.Graph.Type as Graph.Type {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Show as Show {- hmt-base -}

import qualified Music.Theory.Graph.Fgl as Graph.Fgl {- hmt -}
import qualified Music.Theory.Tuning as Tuning {- hmt -}
import qualified Music.Theory.Tuning.Graph.Euler as Euler {- hmt -}
import qualified Music.Theory.Tuning.Scala as Scala {- hmt -}

-- * Fractions

{- | Flip a ratio in (1,2) and multiply by 2.

>>> import Data.Ratio
>>> map r_flip [5%4,3%2,7%4]
[8 % 5,4 % 3,8 % 7]

>>> map r_flip [3/2,5/4,7/4]
[4 % 3,8 % 5,8 % 7]
-}
r_flip :: Rational -> Rational
r_flip n = if n < 1 || n > 2 then error "r_flip" else 1 / n * 2

-- | r = ratio, nrm = normalise
r_nrm :: Rational -> Rational
r_nrm = Tuning.ratio_interval_class_by id

{- | The folded interval from p to q.

>>> r_rel (1,3/2) == 4/3
True
-}
r_rel :: (Rational, Rational) -> Rational
r_rel (p, q) = Tuning.fold_ratio_to_octave_err (p / q)

-- | The interval set /i/ and it's 'r_flip'.
iset_sym :: [Rational] -> [Rational]
iset_sym l = l ++ map r_flip l

-- | Require r to have a perfect octave as last element, and remove it.
rem_oct :: [Rational] -> [Rational]
rem_oct r = if last r /= 2 then error "rem_oct" else List.drop_last r

r_pcset :: [Rational] -> [Int]
r_pcset = sort . map (Tuning.ratio_to_pc 0)

r_pcset_univ :: [Rational] -> [Int]
r_pcset_univ = nub . r_pcset

-- | Does [Rational] construct indicated /pcset/.
r_is_pcset :: [Int] -> [Rational] -> Bool
r_is_pcset pcset = (==) pcset . r_pcset

-- * G

-- | Edges are (v1,v2) where v1 < v2
type G = Graph.Type.Gr Rational

edj_r :: (Rational, Rational) -> Rational
edj_r = r_nrm . r_rel

-- | The graph with vertices /scl_r/ and all edges where the interval (i,j) is in /iset/.
mk_graph :: [Rational] -> [Rational] -> G
mk_graph iset scl_r =
  ( scl_r
  , filter
      (\e -> edj_r e `elem` iset_sym iset)
      [ (p, q)
      | p <- scl_r
      , q <- scl_r
      , p < q
      ]
  )

gen_graph :: Ord v => [Dot.Dot_Meta_Attr] -> Dot.Graph_Pp v e -> [Graph.Fgl.Edge_Lbl v e] -> [String]
gen_graph opt pp es = Graph.Fgl.fgl_to_udot opt pp (Graph.Fgl.g_from_edges_l es)

g_to_dot :: Int -> [(String, String)] -> (Rational -> [(String, String)]) -> G -> [String]
g_to_dot k attr v_attr (_, e_set) =
  let opt =
        [ ("graph:layout", "neato")
        , ("graph:bgcolor", "transparent")
        , ("node:shape", "plaintext")
        , ("node:fontsize", "10")
        , ("node:fontname", "century schoolbook")
        , ("edge:fontsize", "9")
        ]
  in gen_graph
      (opt ++ attr)
      ( \(_, v) -> ("label", Euler.rat_label (k, True) v) : v_attr v
      , \(_, e) -> [("label", Show.rational_pp e)]
      )
      (map (\e -> (e, edj_r e)) e_set)

-- * Scala

mk_graph_scl :: [Rational] -> Scala.Scale -> G
mk_graph_scl iset = mk_graph iset . rem_oct . Scala.scale_ratios_req True

scl_to_dot :: ([Rational], Int, [(String, String)], Rational -> [(String, String)]) -> String -> IO [String]
scl_to_dot (iset, k, attr, v_attr) nm = do
  sc <- Scala.scl_load nm
  let gr = mk_graph_scl iset sc
  return (g_to_dot k attr v_attr gr)

-- * Fgl

graph_to_fgl :: G -> Fgl.Gr Rational Rational
graph_to_fgl (v, e) =
  let fgl_v = zip [0 ..] v
      r_to_v :: Rational -> Int
      r_to_v x = fromJust (List.reverse_lookup x fgl_v)
      fgl_e = map (\(p, q) -> (r_to_v p, r_to_v q, edj_r (p, q))) e
  in Fgl.mkGraph fgl_v fgl_e

mk_graph_fgl :: [Rational] -> [Rational] -> Fgl.Gr Rational Rational
mk_graph_fgl iset = graph_to_fgl . mk_graph iset

{-
-- | List of nodes at /g/ connected to node /r/.
g_edge_list :: G -> Rational -> [Rational]
g_edge_list (_,e) r =
  let f (p,q) = if r == p then Just q else if r == q then Just p else Nothing
  in mapMaybe f e
-}
