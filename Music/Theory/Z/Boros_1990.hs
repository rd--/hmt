-- | James Boros. "Some Properties of the All-Trichord Hexachord".
-- _In Theory Only_, 11(6):19--41, 1990.
module Music.Theory.Z.Boros_1990 where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Numeric {- base -}

import qualified Data.Graph.Inductive.Graph as G {- fgl -}
import qualified Data.Graph.Inductive.Basic as G {- fgl -}
import qualified Data.Graph.Inductive.PatriciaTree as G {- fgl -}
import qualified Data.Graph.Inductive.Query.BFS as G {- fgl -}

import qualified Music.Theory.Array.MD as T
import qualified Music.Theory.Combinations as T
import qualified Music.Theory.Graph.Dot as T
import qualified Music.Theory.Graph.FGL as T
import qualified Music.Theory.List as T
import qualified Music.Theory.Set.List as T
import qualified Music.Theory.Tuple as T
import qualified Music.Theory.Z as T
import qualified Music.Theory.Z.Forte_1973 as T
import qualified Music.Theory.Z.TTO as T

-- * UTIL

singular :: String -> [t] -> t
singular err l =
    case l of
      [x] -> x
      _ -> error ("not singular: " ++ err)

set_eq :: Ord t => [t] -> [t] -> Bool
set_eq p q = T.set p == T.set q

elem_by :: (t -> t -> Bool) -> t -> [t] -> Bool
elem_by f e = any (f e)

-- * TTO

tto_tni_univ :: Integral i => [T.TTO i]
tto_tni_univ = filter (not . T.tto_M) (T.z_tto_univ T.z12)

all_tn :: Integral i => [i] -> [[i]]
all_tn p = map (\n -> map (T.z_add T.z12 n) p) [0..11]

all_tni :: Integral i => [i] -> [[i]]
all_tni p = map (\f -> T.z_tto_apply 5 T.z12 f p) tto_tni_univ

uniq_tni :: Integral i => [i] -> [[i]]
uniq_tni = nub . all_tni

type PC = Int
type PCSET = [PC]
type SC = PCSET

-- > pcset_trs 3 [0,1,9] == [0,3,4]
pcset_trs :: Int -> PCSET -> PCSET
pcset_trs = T.z_tto_tn T.z12

-- | Forte prime forms of the twelve trichordal set classes.
--
-- > length trichords == 12
trichords :: [PCSET]
trichords = filter ((== 3) . length) (T.sc_univ T.z12)

-- | Is a pcset self-inversional, ie. is the inversion of /p/ a transposition of /p/.
--
-- > map (\p -> (p,self_inv p)) trichords
self_inv :: PCSET -> Bool
self_inv p = elem_by set_eq (map (T.z_negate T.z12) p) (all_tn p)

-- | Pretty printer, comma separated.
--
-- > pcset_pp [0,3,7,10] == "0,3,7,10"
pcset_pp :: PCSET -> String
pcset_pp = intercalate "," . map show

-- | Pretty printer, hexadecimal, no separator.
--
-- > pcset_pp_hex [0,3,7,10] == "037A"
pcset_pp_hex :: PCSET -> String
pcset_pp_hex = map toUpper . concat . map (flip showHex "")

-- * ATH

-- | Forte prime form of the all-trichord hexachord.
--
-- > T.sc_name T.z12 ath == "6-Z17"
-- > T.sc "6-Z17" == ath
ath :: PCSET
ath = [0,1,2,4,7,8]

-- | Is /p/ an instance of 'ath'.
is_ath :: PCSET -> Bool
is_ath p = T.forte_prime T.z12 p == ath

-- | Table 1, p.20
--
-- > length ath_univ == 24
ath_univ :: [PCSET]
ath_univ = uniq_tni ath

-- | Calculate 'T.TTO' of pcset, which must be an instance of 'ath'.
--
-- > ath_tni [1,2,3,7,8,11] == T.TTO 3 False True
ath_tni :: PCSET -> T.TTO PC
ath_tni = singular "ath_tni" . filter (not . T.tto_M) . T.z_tto_rel 5 T.z12 ath

-- | Give label for instance of 'ath', prime forms are written H and inversions h.
--
-- > ath_pp [1,2,3,7,8,11] == "h3"
ath_pp :: PCSET -> String
ath_pp p =
    let r = ath_tni p
        h = if T.tto_I r then 'h' else 'H'
    in h : show (T.tto_T r)

-- | The twenty three-element subsets of 'ath'.
--
-- > length ath_trichords == 20
ath_trichords :: [PCSET]
ath_trichords = T.combinations (3::Int) ath

-- | '\\' of 'ath' and /p/, ie. the pitch classes that are in 'ath' and not in /p/.
--
-- > ath_complement [0,1,2] == [4,7,8]
ath_complement :: PCSET -> PCSET
ath_complement p = ath \\ p

-- | /p/ is a pcset, /q/ a sc, calculate pcsets in /q/ that with /p/ form 'ath'.
--
-- > ath_completions [0,1,2] (T.sc "3-3") == [[6,7,10],[4,7,8]]
-- > ath_completions [6,7,10] (T.sc "3-5") == [[1,2,8]]
ath_completions :: PCSET -> SC -> [PCSET]
ath_completions p q =
    let f z = is_ath (p ++ z)
    in filter f (uniq_tni q)

realise_ath_seq :: [PCSET] -> [[PCSET]]
realise_ath_seq sq =
    case sq of
      p:q:sq' -> concatMap (\z -> map (p :) (realise_ath_seq (z : sq'))) (ath_completions p q)
      _ -> [sq]

-- return edges that connect z to nodes at gr in an ATH relation
ath_gr_extend :: [T.EDGE PCSET] -> PCSET -> [T.EDGE PCSET]
ath_gr_extend gr c =
    let f x y = if is_ath (x ++ y) then Just (x,y) else Nothing
        g (p,q) = mapMaybe (f c) [p,q]
    in nub (map T.t2_sort (concatMap g gr))

gr_trs :: Int -> [T.EDGE PCSET] -> [T.EDGE PCSET]
gr_trs n = let f (p,q) = (pcset_trs n p,pcset_trs n q) in map f

-- * TABLES

-- > length table_3 == 20
table_3 :: [((PCSET,SC,T.SC_Name),(PCSET,SC,T.SC_Name))]
table_3 =
    let f p = let q = ath_complement p
                  i x = (x,T.forte_prime T.z12 x,T.sc_name T.z12 x)
              in (i p,i q)
    in map f ath_trichords

pp_tbl :: T.MD_Table String -> [String]
pp_tbl = T.md_table T.md_opt_simple

-- > putStrLn $ unlines $ table_3_md
table_3_md :: [String]
table_3_md =
    let pp = pcset_pp_hex
        f ((p,q,r),(s,t,u)) = [pp p,pp q,r,pp s,pp t,u]
        hdr = ["P","P/SC","P/F","Q=H0-P","Q/SC","Q/F"]
    in pp_tbl (Just hdr,map f table_3)

-- > length table_4 == 10
table_4 :: [((PCSET,PCSET,T.SC_Name),(PCSET,PCSET,T.SC_Name))]
table_4 = nub (map T.t2_sort table_3)

-- > putStrLn $ unlines $ table_4_md
table_4_md :: [String]
table_4_md =
    let pp = pcset_pp_hex
        f ((p,q,r),(s,t,u)) = [pp p ++ "/" ++ pp s,pp q ++ "/" ++ pp t,r ++ "/" ++ u]
        hdr = ["Trichords","Prime Forms","Forte Numbers"]
    in pp_tbl (Just hdr,map f table_4)

table_5 :: [(PCSET,Int)]
table_5 = T.histogram (map (T.forte_prime T.z12) ath_trichords)

-- > putStrLn $ unlines $ table_5_md
table_5_md :: [String]
table_5_md =
    let f (p,q) = [pcset_pp_hex p,show q]
    in pp_tbl (Just ["SC","#ATH"],map f table_5)

table_6 :: [(PCSET,Int,Int)]
table_6 =
    let f (p,n) = (p,n,length (filter (\q -> p `T.is_subset` q) ath_univ))
    in map f table_5

-- > putStrLn $ unlines $ table_6_md
table_6_md :: [String]
table_6_md =
    let f (p,q,r) = [pcset_pp_hex p,show q,show r]
    in pp_tbl (Just ["SC","#H0","#Hn"],map f table_6)

-- * FIGURES

fig_1 :: [T.EDGE PCSET]
fig_1 = map (T.t2_map T.p3_snd) table_4

fig_1_gr :: G.Gr PCSET ()
fig_1_gr = T.g_from_edges fig_1

-- > putStrLn $ unlines $ map (unwords . map pcset_pp) fig_2
fig_2 :: [[PCSET]]
fig_2 =
 let g = G.undir fig_1_gr
     n = G.labNodes g
     n' = filter ((== 2) . G.deg g . fst) n
     c = T.combinations (2::Int) n'
     p = map (\[lhs,rhs] -> G.esp (fst lhs) (fst rhs) g) c
     p' = (filter (not . null) p)
 in map (mapMaybe (\x -> lookup x n)) p'

fig_3 :: [[T.EDGE PCSET]]
fig_3 = map (concatMap (T.adj2 1) . realise_ath_seq) fig_2

fig_3_gr :: [G.Gr PCSET ()]
fig_3_gr = map T.g_from_edges fig_3

fig_4 :: [[T.EDGE PCSET]]
fig_4 =
    let p = concatMap realise_ath_seq fig_2
        q = filter ([0,1,2] `elem`) p
    in map (T.adj2 1) q

fig_5 :: [[T.EDGE PCSET]]
fig_5 =
    let c = [0,4,8]
        f gr = case ath_gr_extend gr c of
                 [] -> Nothing
                 r -> Just (gr ++ r)
        g0 = concat fig_4
    in mapMaybe (\n -> f (gr_trs n g0)) [0 .. 11]

-- * Drawing

uedge_set :: Ord v => [T.EDGE v] -> [T.EDGE v]
uedge_set = nub . map T.t2_sort

-- | Self-inversional pcsets are drawn in a double circle, other pcsets in a circle.
set_shape :: PCSET -> T.DOT_ATTR
set_shape v = ("shape",if self_inv v then "doublecircle" else "circle")

type GR = G.Gr PCSET ()

gr_pp' :: (PCSET -> String) -> T.GR_PP PCSET ()
gr_pp' f = (\v -> [set_shape v,("label",f v)],const [])

gr_pp :: T.GR_PP PCSET ()
gr_pp = gr_pp' pcset_pp

d_fig_1 :: [String]
d_fig_1 = T.fgl_to_udot [] gr_pp fig_1_gr

d_fig_3_g :: GR
d_fig_3_g = T.g_from_edges (uedge_set (concat fig_3))

d_fig_3 :: [String]
d_fig_3 = T.fgl_to_udot [] gr_pp d_fig_3_g

d_fig_3' :: [[String]]
d_fig_3' = map (T.fgl_to_udot [("node:shape","circle")] gr_pp) fig_3_gr

d_fig_4_g :: GR
d_fig_4_g = T.g_from_edges (uedge_set (concat fig_4))

d_fig_4 :: [String]
d_fig_4 = T.fgl_to_udot [] gr_pp d_fig_4_g

d_fig_5_g :: GR
d_fig_5_g = T.g_from_edges (uedge_set (concat fig_5))

d_fig_5 :: [String]
d_fig_5 = T.fgl_to_udot [("edge:len","1.5")] (gr_pp' pcset_pp_hex) d_fig_5_g

d_fig_5_e :: [T.EDGE_L PCSET PCSET]
d_fig_5_e = map (\(p,q) -> ((p,q),p++q)) (uedge_set (concat fig_5))

d_fig_5_g' :: G.Gr PCSET PCSET
d_fig_5_g' = T.g_from_edges_l d_fig_5_e

d_fig_5' :: [String]
d_fig_5' =
    let pp = (\_ -> [("shape","")],\e -> [("label",ath_pp e)])
    in T.fgl_to_udot [("node:shape","point"),("edge:len","1.25")] pp d_fig_5_g'
