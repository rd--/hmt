-- | Graph (fgl) functions.
module Music.Theory.Graph.FGL where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.Map as M {- containers -}

import qualified Data.Graph.Inductive.Graph as G {- fgl -}
import qualified Data.Graph.Inductive.Query as G {- fgl -}
import qualified Data.Graph.Inductive.PatriciaTree as G {- fgl -}

import qualified Control.Monad.Logic as L {- logict -}

import qualified Music.Theory.List as T {- hmt -}

-- | 'L.msum' '.' 'map' 'return'.
ml_from_list :: L.MonadLogic m => [t] -> m t
ml_from_list = L.msum . map return

-- | Synonym for 'G.noNodes'.
g_degree :: G.Gr v e -> Int
g_degree = G.noNodes

-- | 'G.subgraph' of each of 'G.components'.
g_partition :: G.Gr v e -> [G.Gr v e]
g_partition gr = map (\n -> G.subgraph n gr) (G.components gr)

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

type G_NODE_SEL_F v e = G.Gr v e -> G.Node -> [G.Node]

-- | Use /sel_f/ of 'G.pre' for directed graphs and 'G.neighbors' for undirected.
g_hamiltonian_path_ml :: L.MonadLogic m => G_NODE_SEL_F v e -> G.Gr v e -> G.Node -> m [G.Node]
g_hamiltonian_path_ml sel_f gr =
    let n_deg = g_degree gr
        recur r c =
            if length r == n_deg - 1
            then return (c:r)
            else do i <- ml_from_list (sel_f gr c)
                    L.guard (i `notElem` r)
                    recur (c:r) i
    in recur []

-- > map (L.observeAll . ug_hamiltonian_path_ml_0) (g_partition gr)
ug_hamiltonian_path_ml_0 :: L.MonadLogic m => G.Gr v e -> m [G.Node]
ug_hamiltonian_path_ml_0 gr = g_hamiltonian_path_ml G.neighbors gr (G.nodes gr !! 0)

-- * G (from edges)

-- | Edge, no label.
type EDGE v = (v,v)

-- | Graph as set of edges.
type GRAPH v = [EDGE v]

-- | Edge, with label.
type EDGE_L v l = (EDGE v,l)

-- | Graph as set of labeled edges.
type GRAPH_L v l = [EDGE_L v l]

-- | Generate a graph given a set of labelled edges.
g_from_edges_l :: (Eq v,Ord v) => GRAPH_L v e -> G.Gr v e
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
g_from_edges :: Ord v => GRAPH v -> G.Gr v ()
g_from_edges = let f e = (e,()) in g_from_edges_l . map f

-- * Edges

-- | Label sequence of edges starting at one.
e_label_seq :: Integral i => [EDGE v] -> [EDGE_L v i]
e_label_seq = map (\(k,e) -> (e,k)) . zip [1..]

-- | Normalised undirected labeled edge (ie. order nodes).
e_normalise_l :: Ord v => EDGE_L v l -> EDGE_L v l
e_normalise_l ((p,q),r) = ((min p q,max p q),r)

-- | Collate labels for edges that are otherwise equal.
e_collate_l :: Ord v => [EDGE_L v l] -> [EDGE_L v [l]]
e_collate_l = T.collate

-- | 'e_collate_l' of 'e_normalise_l'.
e_collate_normalised_l :: Ord v => [EDGE_L v l] -> [EDGE_L v [l]]
e_collate_normalised_l = e_collate_l . map e_normalise_l

-- | Apply predicate to universe of possible edges.
e_univ_select_edges :: (t -> t -> Bool) -> [t] -> [EDGE t]
e_univ_select_edges f l = [(p,q) | p <- l, q <- l, f p q]

-- | Consider only edges (p,q) where p < q.
e_univ_select_u_edges :: Ord t => (t -> t -> Bool) -> [t] -> [EDGE t]
e_univ_select_u_edges f = let g p q = p < q && f p q in e_univ_select_edges g

-- | Sequence of connected vertices to edges.
--
-- > e_path_to_edges "abcd" == [('a','b'),('b','c'),('c','d')]
e_path_to_edges :: [t] -> [EDGE t]
e_path_to_edges = T.adj2 1
