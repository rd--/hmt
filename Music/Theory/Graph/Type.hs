-- | Graph types.
module Music.Theory.Graph.Type where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.Graph as G {- containers -}

import qualified Music.Theory.List as T {- hmt -}

-- * Type parameterised graph

-- | (vertices,edges)
type GR t = ([t],[(t,t)])

-- | (|V|,|E|)
gr_degree :: GR t -> (Int,Int)
gr_degree (v,e) = (length v,length e)

-- | Re-label graph given table.
gr_relabel :: Eq t => [(t,u)] -> GR t -> GR u
gr_relabel tbl (v,e) =
  let get z = T.lookup_err z tbl
  in (map get v,map (\(p,q) -> (get p,get q)) e)

-- | Un-directed edge equality.
--
-- > e_eq_undir (0,1) (1,0) == True
e_eq_undir :: Eq t => (t,t) -> (t,t) -> Bool
e_eq_undir e0 e1 =
  let swap (i,j) = (j,i)
  in e0 == e1 || e0 == swap e1

-- | Sort edge.
--
-- > map e_sort [(0,1),(1,0)] == [(0,1),(0,1)]
e_sort :: Ord t => (t, t) -> (t, t)
e_sort (i,j) = (min i j,max i j)

-- | List of E to G, derives V from E.
eset_to_gr :: Ord t => [(t,t)] -> GR t
eset_to_gr e =
  let v = sort (nub (concatMap (\(i,j) -> [i,j]) e))
  in (v,e)

-- | Sort v and e.
gr_sort :: Ord t => GR t -> GR t
gr_sort (v,e) = (sort v,sort e)

-- * Int graph

-- | Vertex
type V = Int

-- | Edge
type E = (V,V)

-- | (vertices,edges)
type G = GR V

-- | 'G.Graph' to 'G'.
graph_to_g :: G.Graph -> G
graph_to_g gr = (G.vertices gr,G.edges gr)

-- | 'G' to 'G.Graph'
--
-- > g = ([0,1,2],[(0,1),(0,2),(1,2)])
-- > g == gr_sort (graph_to_g (g_to_graph g))
g_to_graph :: G -> G.Graph
g_to_graph (v,e) = G.buildG (minimum v,maximum v) e

-- | Unlabel graph, make table.
gr_unlabel :: Eq t => GR t -> (G,[(V,t)])
gr_unlabel (v,e) =
  let n = length v
      v' = [0 .. n - 1]
      tbl = zip v' v
      get k = T.reverse_lookup_err k tbl
      e' = map (\(p,q) -> (get p,get q)) e
  in ((v',e'),tbl)

-- | 'g_to_graph' of 'gr_unlabel'.
--
-- > gr = ("abc",[('a','b'),('a','c'),('b','c')])
-- > (g,tbl) = gr_to_graph gr
gr_to_graph :: Eq t => GR t -> (G.Graph,[(V,t)])
gr_to_graph gr =
  let ((v,e),tbl) = gr_unlabel gr
  in (G.buildG (0,length v - 1) e,tbl)

-- * EDG = edge list (zero-indexed)

-- | ((|V|,|E|),[E])
type EDG = ((Int,Int), [E])

-- | Requires V is (0 .. |v| - 1).
edg_to_g :: EDG -> G
edg_to_g ((nv,ne),e) =
  let v = [0 .. nv - 1]
  in if ne /= length e
     then error (show ("edg_to_g",nv,ne,length e))
     else (v,e)

-- | Parse EDG as printed by nauty-listg.
edg_parse :: [String] -> EDG
edg_parse ln =
  let parse_int_list = map read . words
      parse_int_pairs = T.adj2 2 . parse_int_list
      parse_int_pair = T.unlist1_err . parse_int_pairs
  in case ln of
       [m,e] -> (parse_int_pair m,parse_int_pairs e)
       _ -> error "edg_parse"

-- * Adjacencies

-- | Adjacency list
type ADJ t = [(t,[t])]

-- | ADJ to G.
adj_to_gr :: Ord t => ADJ t -> GR t
adj_to_gr adj =
  let e = concatMap (\(i,j) -> zip (repeat i) j) adj
  in eset_to_gr e

-- | G to ADJ.
gr_to_adj :: Ord t => (t -> (t,t) -> Maybe t) -> GR t -> ADJ t
gr_to_adj sel_f (v,e) =
  let f k = (k,sort (mapMaybe (sel_f k) e))
  in filter (\(_,a) -> a /= []) (map f v)

-- | Directed graph to ADJ.
--
-- > g = ([0,1,2,3],[(0,1),(2,1),(0,3),(3,0)])
-- > r = [(0,[1,3]),(2,[1]),(3,[0])]
-- > gr_to_adj_dir g == r
gr_to_adj_dir :: Ord t => GR t -> ADJ t
gr_to_adj_dir =
  let sel_f k (i,j) = if i == k then Just j else Nothing
  in gr_to_adj sel_f

-- | Un-directed graph to ADJ.
--
-- > g = ([0,1,2,3],[(0,1),(2,1),(0,3),(3,0)])
-- > gr_to_adj_undir g == [(0,[1,3,3]),(1,[2])]
gr_to_adj_undir :: Ord t => GR t -> ADJ t
gr_to_adj_undir =
  let sel_f k (i,j) =
        if i == k && j >= k
        then Just j
        else if j == k && i >= k
             then Just i
             else Nothing
  in gr_to_adj sel_f

-- | Adjacency matrix, (|v|,mtx)
type ADJ_MTX = (Int,[[Int]])

{- | EDG to ADJ_MTX for un-directed graph.

> e = ((4,3),[(0,3),(1,3),(2,3)])
> edg_to_adj_mtx_undir e == [[0,0,0,1],[0,0,0,1],[0,0,0,1],[1,1,1,0]]

> e = ((4,4),[(0,1),(0,3),(1,2),(2,3)])
> edg_to_adj_mtx_undir e == [[0,1,0,1],[1,0,1,0],[0,1,0,1],[1,0,1,0]]

-}
edg_to_adj_mtx_undir :: EDG -> ADJ_MTX
edg_to_adj_mtx_undir ((nv,_ne),e) =
  let v = [0 .. nv - 1]
      f i j = case find (e_eq_undir (i,j)) e of
                Nothing -> 0
                _ -> 1
  in (nv,map (\i -> map (f i) v) v)

-- * Labels

-- | Labelled graph.
type LBL v e = ([(V,v)],[(E,e)])

v_label :: v -> LBL v e -> V -> v
v_label def (tbl,_) v = fromMaybe def (lookup v tbl)

v_label_err :: LBL v e -> V -> v
v_label_err = v_label (error "v_label")

e_label :: e -> LBL v e -> E -> e
e_label def (_,tbl) e = fromMaybe def (lookup e tbl)

e_label_err :: LBL v e -> E -> e
e_label_err = e_label (error "e_label")

-- > gr_to_lbl ("ab",[('a','b')]) == ([(0,'a'),(1,'b')],[((0,1),('a','b'))])
gr_to_lbl :: Eq t => GR t -> LBL t (t,t)
gr_to_lbl (v,e) =
  let n = length v
      v' = [0 .. n - 1]
      tbl = zip v' v
      get k = T.reverse_lookup_err k tbl
      e' = map (\(p,q) -> ((get p,get q),(p,q))) e
  in (zip v' v,e')

lbl_delete_edge_labels :: LBL v e -> LBL v ()
lbl_delete_edge_labels (v,e) = (v,map (\(x,_) -> (x,())) e)

gr_to_lbl_ :: Eq t => GR t -> LBL t ()
gr_to_lbl_ = lbl_delete_edge_labels . gr_to_lbl

-- | Construct LBL from set of E, derives V from E.
eset_to_lbl :: Ord t => [(t,t)] -> LBL t ()
eset_to_lbl e =
  let v = nub (sort (concatMap (\(i,j) -> [i,j]) e))
      get_ix z = fromMaybe (error "lbl_recover") (elemIndex z v)
  in (zip [0..] v, map (\(i,j) -> ((get_ix i,get_ix j),())) e)
