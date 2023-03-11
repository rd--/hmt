-- | Graph (fgl) functions.
module Music.Theory.Graph.Fgl where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.Map as M {- containers -}

import qualified Data.Graph.Inductive.Graph as G {- fgl -}
import qualified Data.Graph.Inductive.Query as G {- fgl -}
import qualified Data.Graph.Inductive.PatriciaTree as G {- fgl -}

import qualified Control.Monad.Logic as L {- logict -}

import qualified Music.Theory.Graph.Dot as T {- hmt -}
import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

-- | 'T.Lbl' to FGL graph
lbl_to_fgl :: G.Graph gr => T.Lbl v e -> gr v e
lbl_to_fgl (v,e) = let f ((i,j),k) = (i,j,k) in G.mkGraph v (map f e)

-- | Type-specialised.
lbl_to_fgl_gr :: T.Lbl v e -> G.Gr v e
lbl_to_fgl_gr = lbl_to_fgl

-- | FGL graph to 'T.Lbl'
fgl_to_lbl :: G.Graph gr => gr v e -> T.Lbl v e
fgl_to_lbl gr = (G.labNodes gr,map (\(i,j,k) -> ((i,j),k)) (G.labEdges gr))

-- | Synonym for 'G.noNodes'.
g_degree :: G.Gr v e -> Int
g_degree = G.noNodes

-- | 'G.subgraph' of each of 'G.components'.
g_partition :: G.Gr v e -> [G.Gr v e]
g_partition gr = map (`G.subgraph` gr) (G.components gr)

-- | Find first 'G.Node' with given label.
g_node_lookup :: (Eq v,G.Graph gr) => gr v e -> v -> Maybe G.Node
g_node_lookup gr l = fmap fst (find ((== l) . snd) (G.labNodes gr))

-- | Erroring variant.
g_node_lookup_err :: (Eq v,G.Graph gr) => gr v e -> v -> G.Node
g_node_lookup_err gr = fromMaybe (error "g_node_lookup") . g_node_lookup gr

-- | Set of nodes with given labels, plus all neighbours of these nodes.
-- (impl = implications)
ug_node_set_impl :: (Eq v,G.DynGraph gr) => gr v e -> [v] -> [G.Node]
ug_node_set_impl gr nl =
    let n = map (g_node_lookup_err gr) nl
    in nub (sort (n ++ concatMap (G.neighbors gr) n))

-- * Hamiltonian

-- | Node select function, ie. given a graph /g/ and a node /n/ select a set of related nodes from /g/
type G_Node_Sel_f v e = G.Gr v e -> G.Node -> [G.Node]

-- | 'msum' '.' 'map' 'return'.
ml_from_list :: MonadPlus m => [t] -> m t
ml_from_list = msum . map return

-- | Use /sel_f/ of 'G.pre' for directed graphs and 'G.neighbors' for undirected.
g_hamiltonian_path_ml :: (MonadPlus m, L.MonadLogic m) => G_Node_Sel_f v e -> G.Gr v e -> G.Node -> m [G.Node]
g_hamiltonian_path_ml sel_f gr =
    let n_deg = g_degree gr
        recur r c =
            if length r == n_deg - 1
            then return (c:r)
            else do i <- ml_from_list (sel_f gr c)
                    guard (i `notElem` r)
                    recur (c:r) i
    in recur []

-- | 'g_hamiltonian_path_ml' of 'G.neighbors' starting at first node.
--
-- > map (L.observeAll . ug_hamiltonian_path_ml_0) (g_partition gr)
ug_hamiltonian_path_ml_0 :: (MonadPlus m, L.MonadLogic m) => G.Gr v e -> m [G.Node]
ug_hamiltonian_path_ml_0 gr = g_hamiltonian_path_ml G.neighbors gr (G.nodes gr !! 0)

-- * G (from edges)

-- | Edge, no label.
type Edge v = (v,v)

-- | Edge, with label.
type Edge_Lbl v l = (Edge v,l)

-- | Generate a graph given a set of labelled edges.
g_from_edges_l :: (Eq v,Ord v) => [Edge_Lbl v e] -> G.Gr v e
g_from_edges_l e =
    let n = nub (concatMap (\((lhs,rhs),_) -> [lhs,rhs]) e)
        n_deg = length n
        n_id = [0 .. n_deg - 1]
        m = M.fromList (zip n n_id)
        m_get k = M.findWithDefault (error "g_from_edges: m_get") k m
        e' = map (\((lhs,rhs),label) -> (m_get lhs,m_get rhs,label)) e
    in G.mkGraph (zip n_id n) e'

-- | Variant that supplies '()' as the (constant) edge label.
--
-- > let g = G.mkGraph [(0,'a'),(1,'b'),(2,'c')] [(0,1,()),(1,2,())]
-- > in g_from_edges_ul [('a','b'),('b','c')] == g
g_from_edges :: Ord v => [Edge v] -> G.Gr v ()
g_from_edges = let f e = (e,()) in g_from_edges_l . map f

-- * Edges

-- | Label sequence of edges starting at one.
e_label_seq :: [Edge v] -> [Edge_Lbl v Int]
e_label_seq = zipWith (\k e -> (e,k)) [1..]

-- | Normalised undirected labeled edge (ie. order nodes).
e_normalise_l :: Ord v => Edge_Lbl v l -> Edge_Lbl v l
e_normalise_l ((p,q),r) = ((min p q,max p q),r)

-- | Collate labels for edges that are otherwise equal.
e_collate_l :: Ord v => [Edge_Lbl v l] -> [Edge_Lbl v [l]]
e_collate_l = T.collate

-- | 'e_collate_l' of 'e_normalise_l'.
e_collate_normalised_l :: Ord v => [Edge_Lbl v l] -> [Edge_Lbl v [l]]
e_collate_normalised_l = e_collate_l . map e_normalise_l

-- | Apply predicate to universe of possible edges.
e_univ_select_edges :: (t -> t -> Bool) -> [t] -> [Edge t]
e_univ_select_edges f l = [(p,q) | p <- l, q <- l, f p q]

-- | Consider only edges (p,q) where p < q.
e_univ_select_u_edges :: Ord t => (t -> t -> Bool) -> [t] -> [Edge t]
e_univ_select_u_edges f = let g p q = p < q && f p q in e_univ_select_edges g

-- | Sequence of connected vertices to edges.
--
-- > e_path_to_edges "abcd" == [('a','b'),('b','c'),('c','d')]
e_path_to_edges :: [t] -> [Edge t]
e_path_to_edges = T.adj2 1

-- | Undirected edge equality.
e_undirected_eq :: Eq t => Edge t -> Edge t -> Bool
e_undirected_eq (a,b) (c,d) = (a == c && b == d) || (a == d && b == c)

-- | /any/ of /f/.
elem_by :: (p -> q -> Bool) -> p -> [q] -> Bool
elem_by f = any . f

-- | Is the sequence of vertices a path at the graph, ie. are all
-- adjacencies in the sequence edges.
e_is_path :: Eq t => [Edge t] -> [t] -> Bool
e_is_path e sq =
    case sq of
      p:q:sq' -> elem_by e_undirected_eq (p,q) e && e_is_path e (q:sq')
      _ -> True

-- * Analysis

-- | <https://github.com/ivan-m/Graphalyze/blob/master/Data/Graph/Analysis/Algorithms/Common.hs>
--   Graphalyze has pandoc as a dependency...
pathTree             :: (G.DynGraph g) => G.Decomp g a b -> [[G.Node]]
pathTree (Nothing,_) = []
pathTree (Just ct,g)
    | G.isEmpty g = []
    | null sucs = [[n]]
    | otherwise = (:) [n] . map (n:) . concatMap (subPathTree g') $ sucs
    where
      n = G.node' ct
      sucs = G.suc' ct
      ct' = makeLeaf ct
      g' = ct' G.& g
      subPathTree gr n' = pathTree $ G.match n' gr

-- | Remove all outgoing edges
makeLeaf           :: G.Context a b -> G.Context a b
makeLeaf (p,n,a,_) = (p', n, a, [])
    where p' = filter (\(_,n') -> n' /= n) p

-- * Dot

fgl_to_dot :: G.Graph gr => T.Graph_Type -> [T.Dot_Meta_Attr] -> T.Graph_Pp v e -> gr v e -> [String]
fgl_to_dot typ opt pp gr = T.lbl_to_dot typ opt pp (fgl_to_lbl gr)

fgl_to_udot :: G.Graph gr => [T.Dot_Meta_Attr] -> T.Graph_Pp v e -> gr v e -> [String]
fgl_to_udot opt pp gr = T.lbl_to_udot opt pp (fgl_to_lbl gr)
