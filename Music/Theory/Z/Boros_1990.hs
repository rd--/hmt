{- | James Boros. "Some Properties of the All-Trichord Hexachord".
_In Theory Only_, 11(6):19--41, 1990.
-}
module Music.Theory.Z.Boros_1990 where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Numeric {- base -}

import qualified Data.Graph.Inductive.Basic as Fgl {- fgl -}
import qualified Data.Graph.Inductive.Graph as Fgl {- fgl -}
import qualified Data.Graph.Inductive.PatriciaTree as Fgl {- fgl -}
import qualified Data.Graph.Inductive.Query.BFS as Fgl {- fgl -}

import qualified Music.Theory.Array.Text as Array.Text {- hmt-base -}
import qualified Music.Theory.Combinations as Combinations
import qualified Music.Theory.Graph.Dot as Graph.Dot
import qualified Music.Theory.Graph.Fgl as Graph.Fgl
import qualified Music.Theory.List as List
import qualified Music.Theory.Set.List as Set.List
import qualified Music.Theory.Tuple as Tuple

import qualified Music.Theory.Z as Z {- hmt -}
import qualified Music.Theory.Z.Forte_1973 as Z.Forte_1973
import qualified Music.Theory.Z.Tto as Z.Tto

-- * Util

singular :: String -> [t] -> t
singular err l =
  case l of
    [x] -> x
    _ -> error ("not singular: " ++ err)

set_eq :: Ord t => [t] -> [t] -> Bool
set_eq p q = Set.List.set p == Set.List.set q

elem_by :: (t -> t -> Bool) -> t -> [t] -> Bool
elem_by f e = any (f e)

-- * Tto

tto_tni_univ :: Integral i => [Z.Tto.Tto i]
tto_tni_univ = filter ((== 1) . Z.Tto.tto_M) (Z.Tto.z_tto_univ 5 Z.z12)

all_tn :: Integral i => [i] -> [[i]]
all_tn p = map (\n -> map (Z.z_add Z.z12 n) p) [0 .. 11]

all_tni :: Integral i => [i] -> [[i]]
all_tni p = map (\f -> Z.Tto.z_tto_apply Z.z12 f p) tto_tni_univ

uniq_tni :: Integral i => [i] -> [[i]]
uniq_tni = nub . all_tni

type Pc = Int
type Pcset = [Pc]
type Sc = Pcset

{- | Tranpose

>>> pcset_trs 3 [0,1,9]
[0,3,4]
-}
pcset_trs :: Int -> Pcset -> Pcset
pcset_trs = Z.Tto.z_tto_tn Z.z12

{- | Forte prime forms of the twelve trichordal set classes.

>>> length trichords
12
-}
trichords :: [Pcset]
trichords = filter ((== 3) . length) (Z.Forte_1973.z_sc_univ Z.z12)

{- | Is a pcset self-inversional, ie. is the inversion of /p/ a transposition of /p/.

>>> map (\p -> (p,self_inv p)) trichords
[([0,1,2],True),([0,1,3],False),([0,1,4],False),([0,1,5],False),([0,1,6],False),([0,2,4],True),([0,2,5],False),([0,2,6],False),([0,2,7],True),([0,3,6],True),([0,3,7],False),([0,4,8],True)]
-}
self_inv :: Pcset -> Bool
self_inv p = elem_by set_eq (map (Z.z_negate Z.z12) p) (all_tn p)

{- | Pretty printer, comma separated.

>>> pcset_pp [0,3,7,10]
"0,3,7,10"
-}
pcset_pp :: Pcset -> String
pcset_pp = intercalate "," . map show

{- | Pretty printer, hexadecimal, no separator.

>>> pcset_pp_hex [0,3,7,10]
"037A"
-}
pcset_pp_hex :: Pcset -> String
pcset_pp_hex = map toUpper . concatMap (`showHex` "")

-- * Ath

{- | Forte prime form of the all-trichord hexachord.

>>> Z.Forte_1973.sc_name ath
"6-Z17"

>>> Z.Forte_1973.sc "6-Z17" == ath
True
-}
ath :: Pcset
ath = [0, 1, 2, 4, 7, 8]

-- | Is /p/ an instance of 'ath'.
is_ath :: Pcset -> Bool
is_ath p = Z.Forte_1973.z_forte_prime Z.z12 p == ath

{- | Table 1, p.20

>>> length ath_univ
24
-}
ath_univ :: [Pcset]
ath_univ = uniq_tni ath

{- | Calculate 'Z.Tto' of pcset, which must be an instance of 'ath'.

>>> ath_tni [1,2,3,7,8,11] == Z.Tto.Tto 3 1 True
True
-}
ath_tni :: Pcset -> Z.Tto.Tto Pc
ath_tni = singular "ath_tni" . filter ((== 1) . Z.Tto.tto_M) . Z.Tto.z_tto_rel 5 Z.z12 ath

{- | Give label for instance of 'ath', prime forms are written H and inversions h.

>>> ath_pp [1,2,3,7,8,11]
"h3"
-}
ath_pp :: Pcset -> String
ath_pp p =
  let r = ath_tni p
      h = if Z.Tto.tto_I r then 'h' else 'H'
  in h : show (Z.Tto.tto_T r)

{- | The twenty three-element subsets of 'ath'.

>>> length ath_trichords
20
-}
ath_trichords :: [Pcset]
ath_trichords = Combinations.combinations (3 :: Int) ath

{- | '\\' of 'ath' and /p/, ie. the pitch classes that are in 'ath' and not in /p/.

>>> ath_complement [0,1,2]
[4,7,8]
-}
ath_complement :: Pcset -> Pcset
ath_complement p = ath \\ p

{- | /p/ is a pcset, /q/ a sc, calculate pcsets in /q/ that with /p/ form 'ath'.

>>> ath_completions [0,1,2] (Z.Forte_1973.sc "3-3")
[[6,7,10],[4,7,8]]

>>> ath_completions [6,7,10] (Z.Forte_1973.sc "3-5")
[[1,2,8]]
-}
ath_completions :: Pcset -> Sc -> [Pcset]
ath_completions p q =
  let f z = is_ath (nub (p ++ z))
  in filter f (uniq_tni q)

realise_ath_seq :: [Pcset] -> [[Pcset]]
realise_ath_seq sq =
  case sq of
    p : q : sq' -> concatMap (\z -> map (p :) (realise_ath_seq (z : sq'))) (ath_completions p q)
    _ -> [sq]

-- | Return edges that connect z to nodes at gr in an ATH relation
ath_gr_extend :: [Graph.Fgl.Edge Pcset] -> Pcset -> [Graph.Fgl.Edge Pcset]
ath_gr_extend gr c =
  let f x y = if is_ath (x ++ y) then Just (x, y) else Nothing
      g (p, q) = mapMaybe (f c) [p, q]
  in nub (map Tuple.t2_sort (concatMap g gr))

gr_trs :: Int -> [Graph.Fgl.Edge Pcset] -> [Graph.Fgl.Edge Pcset]
gr_trs n = let f (p, q) = (pcset_trs n p, pcset_trs n q) in map f

-- * Tables

{- | Table 3

>>> length table_3
20
-}
table_3 :: [((Pcset, Sc, Z.Forte_1973.Sc_Name), (Pcset, Sc, Z.Forte_1973.Sc_Name))]
table_3 =
  let f p =
        let q = ath_complement p
            i x = (x, Z.Forte_1973.z_forte_prime Z.z12 x, Z.Forte_1973.sc_name x)
        in (i p, i q)
  in map f ath_trichords

pp_tbl :: Array.Text.Text_Table -> [String]
pp_tbl = Array.Text.table_pp Array.Text.table_opt_simple

{-
>>> putStrLn $ unlines $ table_3_md
-}
table_3_md :: [String]
table_3_md =
  let pp = pcset_pp_hex
      f ((p, q, r), (s, t, u)) = [pp p, pp q, r, pp s, pp t, u]
      hdr = ["P", "P/SC", "P/F", "Q=H0-P", "Q/SC", "Q/F"]
  in pp_tbl (hdr : map f table_3)

{-
>>> length table_4 == 10
-}
table_4 :: [((Pcset, Pcset, Z.Forte_1973.Sc_Name), (Pcset, Pcset, Z.Forte_1973.Sc_Name))]
table_4 = nub (map Tuple.t2_sort table_3)

{-
>>> putStrLn $ unlines $ table_4_md
-}
table_4_md :: [String]
table_4_md =
  let pp = pcset_pp_hex
      f ((p, q, r), (s, t, u)) = [pp p ++ "/" ++ pp s, pp q ++ "/" ++ pp t, r ++ "/" ++ u]
      hdr = ["Trichords", "Prime Forms", "Forte Numbers"]
  in pp_tbl (hdr : map f table_4)

table_5 :: [(Pcset, Int)]
table_5 = List.histogram (map (Z.Forte_1973.z_forte_prime Z.z12) ath_trichords)

{- | Table 5

> putStrLn $ unlines $ table_5_md

@
SC #ATH
--- ----
012    1
013    1
014    2
015    2
016    5
024    1
025    1
026    2
027    1
036    1
037    2
048    1
--- ----
@
-}
table_5_md :: [String]
table_5_md =
  let f (p, q) = [pcset_pp_hex p, show q]
  in pp_tbl (["SC", "#ATH"] : map f table_5)

table_6 :: [(Pcset, Int, Int)]
table_6 =
  let f (p, n) = (p, n, length (filter (\q -> p `List.is_subset` q) ath_univ))
  in map f table_5

{- | Table 6

> putStrLn $ unlines $ table_6_md

@
SC #H0 #Hn
--- --- ---
012   1   2
013   1   1
014   2   2
015   2   2
016   5   5
024   1   2
025   1   1
026   2   2
027   1   2
036   1   2
037   2   2
048   1   6
--- --- ---
@
-}
table_6_md :: [String]
table_6_md =
  let f (p, q, r) = [pcset_pp_hex p, show q, show r]
  in pp_tbl (["SC", "#H0", "#Hn"] : map f table_6)

-- * Figures

fig_1 :: [Graph.Fgl.Edge Pcset]
fig_1 = map (Tuple.t2_map Tuple.p3_snd) table_4

fig_1_gr :: Fgl.Gr Pcset ()
fig_1_gr = Graph.Fgl.g_from_edges fig_1

{- | Fig. 2

> putStrLn $ unlines $ map (unwords . map pcset_pp) fig_2

@
0,1,2 0,1,4 0,1,6 0,2,4
0,1,2 0,1,4 0,1,6 0,3,7 0,2,7
0,1,2 0,1,4 0,1,6 0,2,6 0,3,6
0,1,2 0,1,4 0,1,6 0,4,8
0,2,5 0,1,5 0,1,3
0,2,4 0,1,6 0,3,7 0,2,7
0,2,4 0,1,6 0,2,6 0,3,6
0,2,4 0,1,6 0,4,8
0,2,7 0,3,7 0,1,6 0,2,6 0,3,6
0,2,7 0,3,7 0,1,6 0,4,8
0,3,6 0,2,6 0,1,6 0,4,8
@
-}
fig_2 :: [[Pcset]]
fig_2 =
  let g = Fgl.undir fig_1_gr
      n = Fgl.labNodes g
      n' = filter ((== 2) . Fgl.deg g . fst) n
      c = Combinations.combinations (2 :: Int) n'
      p = map (\l -> let (lhs, rhs) = List.firstSecond l in Fgl.esp (fst lhs) (fst rhs) g) c
      p' = filter (not . null) p
  in map (mapMaybe (`lookup` n)) p'

fig_3 :: [[Graph.Fgl.Edge Pcset]]
fig_3 = map (concatMap (List.adj2 1) . realise_ath_seq) fig_2

fig_3_gr :: [Fgl.Gr Pcset ()]
fig_3_gr = map Graph.Fgl.g_from_edges fig_3

fig_4 :: [[Graph.Fgl.Edge Pcset]]
fig_4 =
  let p = concatMap realise_ath_seq fig_2
      q = filter ([0, 1, 2] `elem`) p
  in map (List.adj2 1) q

fig_5 :: [[Graph.Fgl.Edge Pcset]]
fig_5 =
  let c = [0, 4, 8]
      f gr = case ath_gr_extend gr c of
        [] -> Nothing
        r -> Just (gr ++ r)
      g0 = concat fig_4
  in mapMaybe (\n -> f (gr_trs n g0)) [0 .. 11]

-- * Drawing

uedge_set :: Ord v => [Graph.Fgl.Edge v] -> [Graph.Fgl.Edge v]
uedge_set = nub . map Tuple.t2_sort

-- | Self-inversional pcsets are drawn in a double circle, other pcsets in a circle.
set_shape :: Pcset -> Graph.Dot.Dot_Attr
set_shape v = ("shape", if self_inv v then "doublecircle" else "circle")

type Gr = Fgl.Gr Pcset ()

gr_pp' :: (Pcset -> String) -> Graph.Dot.Graph_Pp Pcset ()
gr_pp' f = (\(_, v) -> [set_shape v, ("label", f v)], const [])

gr_pp :: Graph.Dot.Graph_Pp Pcset ()
gr_pp = gr_pp' pcset_pp

d_fig_1 :: [String]
d_fig_1 = Graph.Fgl.fgl_to_udot [] gr_pp fig_1_gr

d_fig_3_g :: Gr
d_fig_3_g = Graph.Fgl.g_from_edges (uedge_set (concat fig_3))

d_fig_3 :: [String]
d_fig_3 = Graph.Fgl.fgl_to_udot [] gr_pp d_fig_3_g

d_fig_3' :: [[String]]
d_fig_3' = map (Graph.Fgl.fgl_to_udot [("node:shape", "circle")] gr_pp) fig_3_gr

d_fig_4_g :: Gr
d_fig_4_g = Graph.Fgl.g_from_edges (uedge_set (concat fig_4))

d_fig_4 :: [String]
d_fig_4 = Graph.Fgl.fgl_to_udot [] gr_pp d_fig_4_g

d_fig_5_g :: Gr
d_fig_5_g = Graph.Fgl.g_from_edges (uedge_set (concat fig_5))

d_fig_5 :: [String]
d_fig_5 = Graph.Fgl.fgl_to_udot [("edge:len", "1.5")] (gr_pp' pcset_pp_hex) d_fig_5_g

d_fig_5_e :: [Graph.Fgl.Edge_Lbl Pcset Pcset]
d_fig_5_e = map (\(p, q) -> ((p, q), p ++ q)) (uedge_set (concat fig_5))

d_fig_5_g' :: Fgl.Gr Pcset Pcset
d_fig_5_g' = Graph.Fgl.g_from_edges_l d_fig_5_e

d_fig_5' :: [String]
d_fig_5' =
  let pp = (const [("shape", "")], \(_, e) -> [("label", ath_pp e)])
  in Graph.Fgl.fgl_to_udot [("node:shape", "point"), ("edge:len", "1.25")] pp d_fig_5_g'
