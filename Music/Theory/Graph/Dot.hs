-- | Graph (dot) functions.
module Music.Theory.Graph.Dot where

import Control.Monad {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import System.FilePath {- filepath -}
import System.Process {- process -}

import qualified Data.Graph.Inductive.Graph as G {- fgl -}

import qualified Music.Theory.Graph.FGL as T {- hmt -}
import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.List as List {- hmt -}
import qualified Music.Theory.Show as Show {- hmt -}

-- * UTIL

-- | Classify /s/ using a first element predicate, a remainder predicate and a unit predicate.
s_classify :: (t -> Bool) -> (t -> Bool) -> ([t] -> Bool) -> [t] -> Bool
s_classify p q r s =
  case s of
    c0:s' -> p c0 && all q s' && r s
    [] -> False

-- | Symbol rule.
--
-- > map is_symbol ["sym","Sym2","3sym","1",""] == [True,True,False,False,False]
is_symbol :: String -> Bool
is_symbol = s_classify isAlpha isAlphaNum (const True)

-- | Number rule.
--
-- > map is_number ["123","123.45",".25","1.","1.2.3",""] == [True,True,False,True,False,False]
is_number :: String -> Bool
is_number = s_classify isDigit (\c -> isDigit c || c == '.') ((< 2) . length . filter ((==) '.'))

-- | Quote /s/ if 'is_symbol' or 'is_number'.
--
-- > map maybe_quote ["abc","a b c","12","12.3"] == ["abc","\"a b c\"","12","12.3"]
maybe_quote :: String -> String
maybe_quote s = if is_symbol s || is_number s then s else concat ["\"",s,"\""]

-- * ATTR/KEY

type DOT_KEY = String
type DOT_VALUE = String
type DOT_ATTR = (DOT_KEY,DOT_VALUE)

-- | Format 'DOT_ATTR'.
dot_attr_pp :: DOT_ATTR -> String
dot_attr_pp (lhs,rhs) = concat [lhs,"=",maybe_quote rhs]

-- | Format sequence of DOT_ATTR.
--
-- > dot_attr_seq_pp [("layout","neato"),("epsilon","0.0001")]
dot_attr_seq_pp :: [DOT_ATTR] -> String
dot_attr_seq_pp opt =
  if null opt
  then ""
  else concat ["[",intercalate "," (map dot_attr_pp opt),"]"]

-- | Merge attributes, left-biased.
dot_attr_ext :: [DOT_ATTR] -> [DOT_ATTR] -> [DOT_ATTR]
dot_attr_ext = List.assoc_merge

-- | graph|node|edge
type DOT_TYPE = String

-- | (type,[attr])
type DOT_ATTR_SET = (DOT_TYPE,[DOT_ATTR])

-- | Format DOT_ATTR_SET.
--
-- > a = ("graph",[("layout","neato"),("epsilon","0.0001")])
-- > dot_attr_set_pp a == "graph [layout=neato,epsilon=0.0001]"
dot_attr_set_pp :: DOT_ATTR_SET -> String
dot_attr_set_pp (ty,opt) = concat [ty," ",dot_attr_seq_pp opt]

-- | type:attr (type = graph|node|edge)
type DOT_META_KEY = String

type DOT_META_ATTR = (DOT_META_KEY,DOT_VALUE)

-- | Keys are given as "type:attr".
--
-- > dot_key_sep "graph:layout" == ("graph","layout")
dot_key_sep :: DOT_META_KEY -> (DOT_TYPE,DOT_KEY)
dot_key_sep = List.split_on_1_err ":"

-- | Collate DOT_KEY attribute set to DOT_ATTR_SET.
dot_attr_collate :: [DOT_META_ATTR] -> [DOT_ATTR_SET]
dot_attr_collate opt =
    let f (k,v) = let (ty,nm) = dot_key_sep k in (ty,(nm,v))
        c = map f opt
    in List.collate c

-- | Default values for default meta-keys.
--
-- > k = dot_attr_def ("neato","century schoolbook",10,"plaintext")
-- > map dot_attr_set_pp (dot_attr_collate k)
dot_attr_def :: (String,String,Double,String) -> [(DOT_META_ATTR)]
dot_attr_def (ly,fn,fs,sh) =
    [("graph:layout",ly)
    ,("node:fontname",fn)
    ,("node:fontsize",show fs)
    ,("node:shape",sh)]

-- * GRAPH

-- | Graph pretty-printer, (v -> [attr],e -> [attr])
type GR_PP v e = (v -> [DOT_ATTR],e -> [DOT_ATTR])

gr_pp_label_m :: Maybe (v -> DOT_VALUE) -> Maybe (e -> DOT_VALUE) -> GR_PP v e
gr_pp_label_m f_v f_e =
  let lift m e = case m of
                    Nothing -> []
                    Just f -> [("label",f e)]
  in (lift f_v,lift f_e)

-- | Label V & E.
gr_pp_label :: (v -> DOT_VALUE) -> (e -> DOT_VALUE) -> GR_PP v e
gr_pp_label f_v f_e = gr_pp_label_m (Just f_v) (Just f_e)

-- | Label V only.
gr_pp_label_v :: (v -> DOT_VALUE) -> GR_PP v e
gr_pp_label_v f = gr_pp_label_m (Just f) Nothing

-- | br = brace, csl = comma separated list
br_csl_pp :: Show t => [t] -> String
br_csl_pp l =
    case l of
      [e] -> show e
      _ -> List.bracket ('{','}') (intercalate "," (map show l))

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

node_pos_attr :: (Show n, Real n) => (n,n) -> DOT_ATTR
node_pos_attr (x,y) = let pp = Show.real_pp_trunc 2 in ("pos",concat [pp x,",",pp y])

-- | Edge POS attributes are sets of cubic bezier control points.
edge_pos_attr :: Real t => [(t,t)] -> DOT_ATTR
edge_pos_attr pt =
  let r_pp = Show.real_pp_trunc 2
      pt_pp (x,y) = concat [r_pp x,",",r_pp y]
  in ("pos",unwords (map pt_pp pt))

-- | Variant that accepts single cubic bezier data set.
edge_pos_attr_1 :: Real t => ((t,t),(t,t),(t,t),(t,t)) -> DOT_ATTR
edge_pos_attr_1 (p1,p2,p3,p4) = edge_pos_attr [p1,p2,p3,p4]

{-
-- | Vertex position function.
type POS_FN v = (v -> (Int,Int))

g_lift_pos_fn :: (v -> (Int,Int)) -> v -> [DOT_ATTR]
g_lift_pos_fn f v = let (c,r) = f v in [node_pos_attr (c * 100,r * 100)]
-}

lbl_to_dot :: G_TYPE -> [DOT_META_ATTR] -> GR_PP v e -> T.LBL v e -> [String]
lbl_to_dot g_typ opt (v_attr,e_attr) (v,e) =
    let ws s = if null s then "" else " " ++ s
        v_f (k,lbl) = concat [show k,ws (dot_attr_seq_pp (v_attr lbl)),";"]
        e_f ((lhs,rhs),lbl) = concat [show lhs,g_type_to_edge_symbol g_typ,show rhs
                                     ,ws (dot_attr_seq_pp (e_attr lbl)),";"]
    in concat [[g_type_to_string g_typ," g {"]
              ,map dot_attr_set_pp (dot_attr_collate opt)
              ,map v_f v
              ,map e_f e
              ,["}"]]

lbl_to_udot :: [DOT_META_ATTR] -> GR_PP v e -> T.LBL v e -> [String]
lbl_to_udot o pp = lbl_to_dot G_UGRAPH o pp

fgl_to_dot :: G.Graph gr => G_TYPE -> [DOT_META_ATTR] -> GR_PP v e -> gr v e -> [String]
fgl_to_dot typ opt pp gr = lbl_to_dot typ opt pp (T.fgl_to_lbl gr)

fgl_to_udot :: G.Graph gr => [DOT_META_ATTR] -> GR_PP v e -> gr v e -> [String]
fgl_to_udot opt pp gr = lbl_to_udot opt pp (T.fgl_to_lbl gr)

-- * DOT-PROCESS

{- | Run /dot/ to generate a file type based on the output file extension
   (ie. .svg, .png, .jpeg, .gif)

   /-n/ must be given to not run the layout algorithm and to use position data in the /dot/ file.
-}
dot_to_ext :: [String] -> FilePath -> FilePath -> IO ()
dot_to_ext opt dot_fn ext_fn =
  let arg = opt ++ ["-T",tail (takeExtension ext_fn),"-o",ext_fn,dot_fn]
  in void (rawSystem "dot" arg)

-- | Alias for 'dot_to_ext'
dot_to_svg :: [String] -> FilePath -> FilePath -> IO ()
dot_to_svg = dot_to_ext

