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

-- | Label graph given labeling table.
gr_label :: Eq t => [(t,u)] -> GR t -> GR u
gr_label tbl (v,e) =
  let get z = T.lookup_err z tbl
  in (map get v,map (\(p,q) -> (get p,get q)) e)

-- | Unlabel graph, make labeling table.
gr_unlabel :: Eq t => GR t -> (G,[(V,t)])
gr_unlabel (v,e) =
  let n = length v
      v' = [0 .. n - 1]
      tbl = zip v' v
      get k = T.reverse_lookup_err k tbl
      e' = map (\(p,q) -> (get p,get q)) e
  in ((v',e'),tbl)

gr_to_graph :: Eq t => GR t -> (G.Graph,[(V,t)])
gr_to_graph gr =
  let ((v,e),tbl) = gr_unlabel gr
  in (G.buildG (0,length v - 1) e,tbl)

-- | Sort edge.
e_sort :: Ord t => (t, t) -> (t, t)
e_sort (i,j) = (min i j,max i j)

-- | List of E to G, derives V from E.
eset_to_gr :: Ord t => [(t,t)] -> GR t
eset_to_gr e =
  let v = sort (nub (concatMap (\(i,j) -> [i,j]) e))
  in (v,e)

-- * Int graph

-- | Vertex
type V = Int

-- | Edge
type E = (V,V)

-- | (vertices,edges)
type G = GR V

graph_to_g :: G.Graph -> G
graph_to_g gr = (G.vertices gr,G.edges gr)

-- * EDG = edge list (zero-indexed)

-- | ((|V|,|E|),[E])
type EDG = ((Int,Int), [E])

-- | Requires V is (0 .. |v| - 1).
edg_to_g :: EDG -> G
edg_to_g ((nv,ne),e) =
  let v = [0 .. nv - 1]
  in if ne /= length e
     then error (show ("el_to_gr",nv,ne,length e))
     else (v,e)

-- | Parse EDG as printed by nauty-listg.
edg_parse :: [String] -> EDG
edg_parse ln =
  let parse_int_list = map read . words
      parse_int_pairs = T.adj2 2 . parse_int_list
      parse_int_pair = T.unlist1_err . parse_int_pairs
  in case ln of
       [m,e] -> (parse_int_pair m,parse_int_pairs e)
       _ -> error "parse_e"

-- * Adjacencies

-- | Adjacency list
type ADJ = [(V,[V])]

adj_to_g :: ADJ -> G
adj_to_g adj =
  let e = concatMap (\(i,j) -> zip (repeat i) j) adj
  in eset_to_gr e

-- > g_to_adj ([0,1,2,3],[(0,1),(1,2)]) == [(0,[1]),(1,[2])]
g_to_adj :: G -> ADJ
g_to_adj (v,e) =
  let sel k (i,j) = if i == k && j >= k
                    then Just j
                    else if j == k && i >= k
                         then Just i
                         else Nothing
      f k = (k,nub (sort (mapMaybe (sel k) e)))
  in filter (\(_,a) -> a /= []) (map f v)
