-- | Graph (fgl) functions.
module Music.Theory.Graph.Fgl where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.Map as Map {- containers -}

import qualified Data.Graph.Inductive.Graph as Fgl {- fgl -}
import qualified Data.Graph.Inductive.PatriciaTree as Fgl {- fgl -}
import qualified Data.Graph.Inductive.Query as Fgl {- fgl -}

import qualified Control.Monad.Logic as Logic {- logict -}

import qualified Music.Theory.Graph.Dot as Dot {- hmt -}
import qualified Music.Theory.Graph.Type as Graph {- hmt -}
import qualified Music.Theory.List as List {- hmt -}

-- | 'Graph.Lbl' to Fgl graph
lbl_to_fgl :: Fgl.Graph gr => Graph.Lbl v e -> gr v e
lbl_to_fgl (v, e) = let f ((i, j), k) = (i, j, k) in Fgl.mkGraph v (map f e)

-- | Type-specialised.
lbl_to_fgl_gr :: Graph.Lbl v e -> Fgl.Gr v e
lbl_to_fgl_gr = lbl_to_fgl

-- | Fgl graph to 'Graph.Lbl'
fgl_to_lbl :: Fgl.Graph gr => gr v e -> Graph.Lbl v e
fgl_to_lbl gr = (Fgl.labNodes gr, map (\(i, j, k) -> ((i, j), k)) (Fgl.labEdges gr))

-- | Synonym for 'Fgl.noNodes'.
g_degree :: Fgl.Gr v e -> Int
g_degree = Fgl.noNodes

-- | 'Fgl.subgraph' of each of 'Fgl.components'.
g_partition :: Fgl.Gr v e -> [Fgl.Gr v e]
g_partition gr = map (`Fgl.subgraph` gr) (Fgl.components gr)

-- | Find first 'Fgl.Node' with given label.
g_node_lookup :: (Eq v, Fgl.Graph gr) => gr v e -> v -> Maybe Fgl.Node
g_node_lookup gr l = fmap fst (find ((== l) . snd) (Fgl.labNodes gr))

-- | Erroring variant.
g_node_lookup_err :: (Eq v, Fgl.Graph gr) => gr v e -> v -> Fgl.Node
g_node_lookup_err gr = fromMaybe (error "g_node_lookup") . g_node_lookup gr

{- | Set of nodes with given labels, plus all neighbours of these nodes.
(impl = implications)
-}
ug_node_set_impl :: (Eq v, Fgl.DynGraph gr) => gr v e -> [v] -> [Fgl.Node]
ug_node_set_impl gr nl =
  let n = map (g_node_lookup_err gr) nl
  in nub (sort (n ++ concatMap (Fgl.neighbors gr) n))

-- * Hamiltonian

-- | Node select function, ie. given a graph /g/ and a node /n/ select a set of related nodes from /g/
type G_Node_Sel_f v e = Fgl.Gr v e -> Fgl.Node -> [Fgl.Node]

-- | 'msum' '.' 'map' 'return'.
ml_from_list :: MonadPlus m => [t] -> m t
ml_from_list = msum . map return

-- | Use /sel_f/ of 'Fgl.pre' for directed graphs and 'Fgl.neighbors' for undirected.
g_hamiltonian_path_ml :: (MonadPlus m, Logic.MonadLogic m) => G_Node_Sel_f v e -> Fgl.Gr v e -> Fgl.Node -> m [Fgl.Node]
g_hamiltonian_path_ml sel_f gr =
  let n_deg = g_degree gr
      recur r c =
        if length r == n_deg - 1
          then return (c : r)
          else do
            i <- ml_from_list (sel_f gr c)
            guard (i `notElem` r)
            recur (c : r) i
  in recur []

{- | 'g_hamiltonian_path_ml' of 'Fgl.neighbors' starting at first node.

> map (Logic.observeAll . ug_hamiltonian_path_ml_0) (g_partition gr)
-}
ug_hamiltonian_path_ml_0 :: (MonadPlus m, Logic.MonadLogic m) => Fgl.Gr v e -> m [Fgl.Node]
ug_hamiltonian_path_ml_0 gr = g_hamiltonian_path_ml Fgl.neighbors gr (Fgl.nodes gr !! 0)

-- * G (from edges)

-- | Edge, no label.
type Edge v = (v, v)

-- | Edge, with label.
type Edge_Lbl v l = (Edge v, l)

-- | Generate a graph given a set of labelled edges.
g_from_edges_l :: (Eq v, Ord v) => [Edge_Lbl v e] -> Fgl.Gr v e
g_from_edges_l e =
  let n = nub (concatMap (\((lhs, rhs), _) -> [lhs, rhs]) e)
      n_deg = length n
      n_id = [0 .. n_deg - 1]
      m = Map.fromList (zip n n_id)
      m_get k = Map.findWithDefault (error "g_from_edges: m_get") k m
      e' = map (\((lhs, rhs), label) -> (m_get lhs, m_get rhs, label)) e
  in Fgl.mkGraph (zip n_id n) e'

{- | Variant that supplies '()' as the (constant) edge label.

>>> let g = Fgl.mkGraph [(0,'a'),(1,'b'),(2,'c')] [(0,1,()),(1,2,())]
>>> g_from_edges [('a','b'),('b','c')] == g
True
-}
g_from_edges :: Ord v => [Edge v] -> Fgl.Gr v ()
g_from_edges = let f e = (e, ()) in g_from_edges_l . map f

-- * Edges

-- | Label sequence of edges starting at one.
e_label_seq :: [Edge v] -> [Edge_Lbl v Int]
e_label_seq = zipWith (\k e -> (e, k)) [1 ..]

-- | Normalised undirected labeled edge (ie. order nodes).
e_normalise_l :: Ord v => Edge_Lbl v l -> Edge_Lbl v l
e_normalise_l ((p, q), r) = ((min p q, max p q), r)

-- | Collate labels for edges that are otherwise equal.
e_collate_l :: Ord v => [Edge_Lbl v l] -> [Edge_Lbl v [l]]
e_collate_l = List.collate

-- | 'e_collate_l' of 'e_normalise_l'.
e_collate_normalised_l :: Ord v => [Edge_Lbl v l] -> [Edge_Lbl v [l]]
e_collate_normalised_l = e_collate_l . map e_normalise_l

-- | Apply predicate to universe of possible edges.
e_univ_select_edges :: (t -> t -> Bool) -> [t] -> [Edge t]
e_univ_select_edges f l = [(p, q) | p <- l, q <- l, f p q]

-- | Consider only edges (p,q) where p < q.
e_univ_select_u_edges :: Ord t => (t -> t -> Bool) -> [t] -> [Edge t]
e_univ_select_u_edges f = let g p q = p < q && f p q in e_univ_select_edges g

{- | Sequence of connected vertices to edges.

>>> e_path_to_edges "abcd"
[('a','b'),('b','c'),('c','d')]
-}
e_path_to_edges :: [t] -> [Edge t]
e_path_to_edges = List.adj2 1

-- | Undirected edge equality.
e_undirected_eq :: Eq t => Edge t -> Edge t -> Bool
e_undirected_eq (a, b) (c, d) = (a == c && b == d) || (a == d && b == c)

-- | /any/ of /f/.
elem_by :: (p -> q -> Bool) -> p -> [q] -> Bool
elem_by f = any . f

{- | Is the sequence of vertices a path at the graph, ie. are all
adjacencies in the sequence edges.
-}
e_is_path :: Eq t => [Edge t] -> [t] -> Bool
e_is_path e sq =
  case sq of
    p : q : sq' -> elem_by e_undirected_eq (p, q) e && e_is_path e (q : sq')
    _ -> True

-- * Analysis

{- | <https://github.com/ivan-m/Graphalyze/blob/master/Data/Graph/Analysis/Algorithms/Common.hs>
  Graphalyze has pandoc as a dependency...
-}
pathTree :: (Fgl.DynGraph g) => Fgl.Decomp g a b -> [[Fgl.Node]]
pathTree (Nothing, _) = []
pathTree (Just ct, g)
  | Fgl.isEmpty g = []
  | null sucs = [[n]]
  | otherwise = (:) [n] . map (n :) . concatMap (subPathTree g') $ sucs
 where
  n = Fgl.node' ct
  sucs = Fgl.suc' ct
  ct' = makeLeaf ct
  g' = ct' Fgl.& g
  subPathTree gr n' = pathTree $ Fgl.match n' gr

-- | Remove all outgoing edges
makeLeaf :: Fgl.Context a b -> Fgl.Context a b
makeLeaf (p, n, a, _) = (p', n, a, [])
 where
  p' = filter (\(_, n') -> n' /= n) p

-- * Dot

fgl_to_dot :: Fgl.Graph gr => Dot.Graph_Type -> [Dot.Dot_Meta_Attr] -> Dot.Graph_Pp v e -> gr v e -> [String]
fgl_to_dot typ opt pp gr = Dot.lbl_to_dot typ opt pp (fgl_to_lbl gr)

fgl_to_udot :: Fgl.Graph gr => [Dot.Dot_Meta_Attr] -> Dot.Graph_Pp v e -> gr v e -> [String]
fgl_to_udot opt pp gr = Dot.lbl_to_udot opt pp (fgl_to_lbl gr)
