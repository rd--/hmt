-- | Graph (dot) functions.
module Music.Theory.Graph.Dot where

import Data.Char {- base -}
import Data.List {- base -}

import qualified Data.Graph.Inductive.Graph as G {- fgl -}
import qualified Data.Graph.Inductive.PatriciaTree as G {- fgl -}

import qualified Music.Theory.List as T {- hmt -}

-- * UTIL

-- | Separate at element.
--
-- > sep1 ':' "graph:layout"
sep1 :: Eq t => t -> [t] -> ([t],[t])
sep1 e l =
    case break (== e) l of
      (p,_:q) -> (p,q)
      _ -> error "sep1"

s_classify :: (t -> Bool) -> (t -> Bool) -> [t] -> Bool
s_classify p q s =
  case s of
    c0:s' -> p c0 && all q s'
    [] -> False

-- > map is_symbol ["sym","sym2","3sym",""] == [True,True,False,False]
is_symbol :: String -> Bool
is_symbol = s_classify isAlpha isAlphaNum

-- > map is_number ["123","123.45",".25","1.1.1",""] == [True,True,False,True,False]
is_number :: String -> Bool
is_number = s_classify isDigit (\c -> isDigit c || c == '.')

-- | Quote /s/ if it includes white space.
--
-- > map maybe_quote ["abc","a b c","12","12.3"] == ["abc","\"a b c\"","12","12.3"]
maybe_quote :: String -> String
maybe_quote s = if is_symbol s || is_number s then s else concat ["\"",s,"\""]

-- | Left biased union of association lists /p/ and /q/.
--
-- > assoc_union [(5,"a"),(3,"b")] [(5,"A"),(7,"C")] == [(5,"a"),(3,"b"),(7,"C")]
assoc_union :: Eq k => [(k,v)] -> [(k,v)] -> [(k,v)]
assoc_union p q =
    let p_k = map fst p
        q' = filter ((`notElem` p_k) . fst) q
    in p ++ q'

-- * ATTR

-- | area:opt (area = graph|node|edge)
type DOT_KEY = String
type DOT_OPT = String
type DOT_VALUE = String
type DOT_ATTR = (DOT_OPT,DOT_VALUE)
type DOT_ATTR_SET = (String,[DOT_ATTR])

-- > dot_key_sep "graph:layout"
dot_key_sep :: String -> (String,String)
dot_key_sep = sep1 ':'

dot_attr_pp :: DOT_ATTR -> String
dot_attr_pp (lhs,rhs) = concat [lhs,"=",maybe_quote rhs]

dot_attr_seq_pp :: [DOT_ATTR] -> String
dot_attr_seq_pp opt =
  if null opt
  then ""
  else concat [" [",intercalate "," (map dot_attr_pp opt),"]"]

dot_attr_set_pp :: DOT_ATTR_SET -> String
dot_attr_set_pp (ty,opt) = ty ++ dot_attr_seq_pp opt

dot_attr_collate :: [DOT_ATTR] -> [DOT_ATTR_SET]
dot_attr_collate opt =
    let f (k,v) = let (ty,nm) = dot_key_sep k in (ty,(nm,v))
        c = map f opt
    in T.collate c

dot_attr_ext :: [DOT_ATTR] -> [DOT_ATTR] -> [DOT_ATTR]
dot_attr_ext = assoc_union

-- > map dot_attr_set_pp (dot_attr_collate dot_attr_def)
dot_attr_def :: [DOT_ATTR]
dot_attr_def =
    [("graph:layout","neato")
    ,("graph:epsilon","0.000001")
    ,("node:shape","plaintext")
    ,("node:fontsize","10")
    ,("node:fontname","century schoolbook")]

-- * GRAPH

-- | Graph pretty-printer, (node->attr,edge->attr)
type GR_PP v e = (v -> [DOT_ATTR],e -> [DOT_ATTR])

gr_pp_lift_node_f :: (v -> String) -> GR_PP v e
gr_pp_lift_node_f f = (\v -> [("label",f v)], const [])

gr_pp_id_show :: Show e => GR_PP String e
gr_pp_id_show = (\v -> [("label",v)],\e -> [("label",show e)])

-- | br = brace, csl = comma separated list
br_csl_pp :: Show t => [t] -> String
br_csl_pp l =
    case l of
      [e] -> show e
      _ -> T.bracket ('{','}') (intercalate "," (map show l))

gr_pp_id_br_csl :: Show e => GR_PP String [e]
gr_pp_id_br_csl = (\v -> [("label",v)],\e -> [("label",br_csl_pp e)])

-- | Graph type, directed or un-directed.
data G_TYPE = G_DIGRAPH | G_UGRAPH

g_type_to_string :: G_TYPE -> String
g_type_to_string ty =
    case ty of
      G_DIGRAPH -> "digraph"
      G_UGRAPH -> "graph"

g_type_to_edge_symbol :: G_TYPE -> String
g_type_to_edge_symbol ty =
    case ty of
      G_DIGRAPH -> " -> "
      G_UGRAPH -> " -- "

-- | Vertex position function.
type POS_FN v = (v -> (Int,Int))

g_lift_pos_fn :: (v -> (Int,Int)) -> v -> [DOT_ATTR]
g_lift_pos_fn f v = let (c,r) = f v in [("pos",show (c * 100) ++ "," ++ show (r * 100))]

g_to_dot :: G_TYPE -> [DOT_ATTR] -> GR_PP v e -> G.Gr v e -> [String]
g_to_dot g_typ opt (v_attr,e_attr) gr =
    let v_f (k,n) = concat [show k,dot_attr_seq_pp (v_attr n),";"]
        e_f (lhs,rhs,e) = concat [show lhs,g_type_to_edge_symbol g_typ,show rhs
                                 ,dot_attr_seq_pp (e_attr e),";"]
    in concat [[g_type_to_string g_typ," g {"]
              ,map dot_attr_set_pp (dot_attr_collate (assoc_union opt dot_attr_def))
              ,map v_f (G.labNodes gr)
              ,map e_f (G.labEdges gr)
              ,["}"]]

g_to_udot :: [DOT_ATTR] -> GR_PP v e -> G.Gr v e -> [String]
g_to_udot o pp = g_to_dot G_UGRAPH o pp
