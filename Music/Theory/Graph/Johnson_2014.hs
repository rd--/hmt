-- | Tom Johnson. /Other Harmony: Beyond Tonal and Atonal/. Editions 75, 2014.
module Music.Theory.Graph.Johnson_2014 where

import Control.Monad {- base -}
import Data.Int {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Control.Monad.Logic as L {- logict -}
import qualified Data.Map as M {- containers -}
import qualified Data.Graph.Inductive as G {- fgl -}
--import qualified Data.Graph.Inductive.PatriciaTree as G {- fgl -}

import qualified Music.Theory.Combinations as T {- hmt-base -}
import qualified Music.Theory.List as T {- hmt-base -}
import qualified Music.Theory.Tuple as T {- hmt-base -}

import qualified Music.Theory.Graph.Dot as T {- hmt -}
import qualified Music.Theory.Graph.Fgl as T {- hmt -}
import qualified Music.Theory.Key as T {- hmt -}
import qualified Music.Theory.Pitch.Note as T {- hmt -}
import qualified Music.Theory.Set.List as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}
import qualified Music.Theory.Tuning.Graph.Euler as T {- hmt -}
import qualified Music.Theory.Z as T {- hmt -}
import qualified Music.Theory.Z.Forte_1973 as T {- hmt -}
import qualified Music.Theory.Z.Tto as T {- hmt -}
import qualified Music.Theory.Z.Sro as T {- hmt -}

-- * Common

type Z12 = Int8

dif :: Num a => (a, a) -> a
dif = uncurry (-)

absdif :: Num a => (a, a) -> a
absdif = abs . dif

-- | interval (0,11) to interval class (0,6)
i_to_ic :: (Num a, Ord a) => a -> a
i_to_ic n = if n > 6 then 12 - n else n

p2_and :: (t -> u -> Bool) -> (t -> u -> Bool) -> t -> u -> Bool
p2_and p q i j = p i j && q i j

-- | degree of intersection
doi :: Eq t => [t] -> [t] -> Int
doi p = length . intersect p

doi_of :: Eq t => Int -> [t] -> [t] -> Bool
doi_of n p = (==) n . doi p

-- | The sum of the pointwise absolute difference.
loc_dif :: Num t => [t] -> [t] -> t
loc_dif p q = let f i j = abs (i - j) in sum (zipWith f p q)

loc_dif_of :: (Eq t, Num t) => t -> [t] -> [t] -> Bool
loc_dif_of n p q = loc_dif p q == n

loc_dif_in :: (Eq t, Num t) => [t] -> [t] -> [t] -> Bool
loc_dif_in n p q = loc_dif p q `elem` n

-- | The number of places that are, pointwise, not equal.
--
-- > loc_dif_n "test" "pest" == 1
loc_dif_n :: (Eq t,Num i) => [t] -> [t] -> i
loc_dif_n p q =
    let f i j = if i == j then 0 else 1
    in sum (zipWith f p q)

loc_dif_n_of :: Eq t => Int -> [t] -> [t] -> Bool
loc_dif_n_of n p q = loc_dif_n p q == n

-- > min_vl [6,11,13] [6,10,14] == 2
min_vl :: (Num a,Ord a) => [a] -> [a] -> a
min_vl p q =
    let f x = sum (zipWith (curry absdif) p x)
    in minimum (map f (permutations q))

min_vl_of :: (Num a, Ord a) => a -> [a] -> [a] -> Bool
min_vl_of n p q = min_vl p q == n

min_vl_in :: (Num a, Ord a) => [a] -> [a] -> [a] -> Bool
min_vl_in n p q = min_vl p q `elem` n

combinations2 :: Ord t => [t] -> [(t, t)]
combinations2 p = [(i,j) | i <- p, j <- p, i < j]

set_pp :: Show t => [t] -> String
set_pp = intercalate "," . map show

tto_rel_to :: Integral t => T.Z t -> [t] -> [t] -> [T.Tto t]
tto_rel_to z p q = T.z_tto_rel 5 z (T.set p) (T.set q)

set_pp_tto_rel :: (Integral t, Show t) => T.Z t -> [t] -> [t] -> String
set_pp_tto_rel z p = intercalate "," . map T.tto_pp . tto_rel_to z p

-- * Map

m_get :: Ord k => M.Map k v -> k -> v
m_get m i = fromMaybe (error "get") (M.lookup i m)

-- | degree of intersection
m_doi_of :: M.Map Int [Z12] -> Int -> Int -> Int -> Bool
m_doi_of m n p q = doi_of n (m_get m p) (m_get m q)

-- * Edge

-- | Add /k/ as prefix to both left and right hand sides of edge.
e_add_id :: k -> [(t,u)] -> [((k,t),(k,u))]
e_add_id k = map (\(lhs,rhs) -> ((k,lhs),(k,rhs)))

gen_edges :: (t -> t -> Bool) -> [t] -> [(t,t)]
gen_edges f l = [(p,q) | p <- l, q <- l, f p q]

gen_u_edges :: Ord a => (a -> a -> Bool) -> [a] -> [(a, a)]
gen_u_edges = T.e_univ_select_u_edges

-- * Graph

oh_def_opt :: [T.Dot_Meta_Attr]
oh_def_opt =
  [("graph:layout","neato")
  ,("graph:epsilon","0.000001")
  ,("node:shape","plaintext")
  ,("node:fontsize","10")
  ,("node:fontname","century schoolbook")]

gen_graph :: Ord v => [T.Dot_Meta_Attr] -> T.Graph_Pp v e -> [T.Edge_Lbl v e] -> [String]
gen_graph opt pp es = T.fgl_to_udot (oh_def_opt ++ opt) pp (T.g_from_edges_l es)

gen_graph_ul :: Ord v => [T.Dot_Meta_Attr] -> (v -> String) -> [T.Edge v] -> [String]
gen_graph_ul opt pp es = T.fgl_to_udot (oh_def_opt ++ opt) (T.gr_pp_label_v pp) (T.g_from_edges es)

gen_graph_ul_ty :: Ord v => String -> (v -> String) -> [T.Edge v] -> [String]
gen_graph_ul_ty ty = gen_graph_ul [("graph:layout",ty)]

gen_flt_graph_pp :: Ord t => [T.Dot_Meta_Attr] -> ([t] -> String) -> ([t] -> [t] -> Bool) -> [[t]] -> [String]
gen_flt_graph_pp opt pp f p = gen_graph_ul opt pp (gen_u_edges f p)

gen_flt_graph :: (Ord t, Show t) => [T.Dot_Meta_Attr] -> ([t] -> [t] -> Bool) -> [[t]] -> [String]
gen_flt_graph opt = gen_flt_graph_pp opt set_pp

-- * P.12

-- > circ_5 12 0 == [0,7,2,9,4,11,6,1,8,3,10,5]
circ_5 :: Integral a => Int -> a -> [a]
circ_5 l n = take l (iterate (T.z_mod T.z12 . (+ 7)) (T.z_mod T.z12 n))

all_pairs :: [t] -> [u] -> [(t,u)]
all_pairs x y = [(p,q) | p <- x, q <- y]

adj :: [t] -> [(t,t)]
adj = T.adj2 1

adj_cyc :: [t] -> [(t,t)]
adj_cyc = T.adj2_cyclic 1

p12_c5_eset :: [(Int,Int)]
p12_c5_eset =
    let l1 = circ_5 4 9 -- [9,4,11,6]
        l2 = circ_5 5 10 -- [10,5,0,7,2]
        l3 = circ_5 3 1 -- [1,8,3]
        align p q = filter ((== 4) . T.z_mod T.z12 . dif) (all_pairs p q)
    in concatMap adj [l1,l2,l3] ++ align l1 l2 ++ align l2 l3

e_add_label :: (T.Edge v -> l) -> [T.Edge v] -> [T.Edge_Lbl v l]
e_add_label f = let g (p,q) = ((p,q),f (p,q)) in map g

p12_c5_gr :: [String]
p12_c5_gr =
    let o = [("graph:start","187623")
            ,("node:fontsize","10")
            ,("edge:fontsize","9")]
        e_l = e_add_label (i_to_ic . absdif) p12_c5_eset
    in gen_graph o (\(_,v) -> [("label",T.pc_pp v)],\(_,e) -> [("label",show e)]) e_l

-- > T.euler_plane_r p12_euler_plane == [1/1,16/15,9/8,6/5,5/4,4/3,45/32,3/2,8/5,5/3,16/9,15/8]
p12_euler_plane :: T.Euler_Plane Rational
p12_euler_plane =
    let f = T.fold_ratio_to_octave_err
        l1 = T.tun_seq 4 (3/2) (f (1 * 2/3 * 5/4))
        l2 = T.tun_seq 5 (3/2) (f (1 * 2/3 * 2/3))
        l3 = T.tun_seq 3 (3/2) (f (1 * 2/3 * 4/5))
        (c1,c2) = T.euler_align_rat (5/4,5/4) (l1,l2,l3)
    in ([l1,l2,l3],c1 ++ c2)

p12_euler_plane_gr :: [String]
p12_euler_plane_gr = T.euler_plane_to_dot_rat (0,True) p12_euler_plane

-- * P.14

p14_eset :: ([(Int, Int)], [(Int, Int)], [(Int, Int)])
p14_eset =
  let univ = [0 .. 11]
      trs n = map (T.z_mod T.z12 . (+ n))
      e_par = zip univ univ
      e_rel = zip univ (trs 9 univ)
      e_med = zip univ (trs 4 univ)
  in (e_par,e_rel,e_med)

p14_mk_e :: [(Int, Int)] -> [(T.Key,T.Key)]
p14_mk_e =
  let pc_to_key m pc = let (n,a) = fromMaybe (error "p14_mk_e?") (T.pc_to_note_alteration_ks pc) in (n,a,m)
      e_lift (lhs,rhs) = (pc_to_key T.Major_Mode lhs,pc_to_key T.Minor_Mode rhs)
  in map e_lift

p14_edges_u :: [(T.Key,T.Key)]
p14_edges_u =
  let (e_par,e_rel,e_med) = p14_eset
  in p14_mk_e (concat [e_par,e_rel,e_med])

p14_edges :: [(T.Key,T.Key)]
p14_edges =
  let (e_par,e_rel,e_med) = p14_eset
      del_par = [10]
      del_rel = [5,6]
      del_med = [2,5,8,11]
      rem_set r = filter (\(lhs,_) -> lhs `notElem` r)
      e_mod = concat [rem_set del_par e_par,rem_set del_rel e_rel,rem_set del_med e_med]
  in p14_mk_e e_mod

p14_mk_gr :: [T.Dot_Meta_Attr] -> [T.Edge T.Key] -> [String]
p14_mk_gr opt e =
    let opt' = ("graph:start","168732") : opt
        pp = T.gr_pp_label_v T.key_lc_uc_pp
        gr = T.g_from_edges e
    in T.fgl_to_udot opt' pp gr

p14_gr_u :: [String]
p14_gr_u =
  p14_mk_gr
  [("edge:len","1.5")
  ,("edge:fontsize","6")
  ,("node:shape","box")
  ,("node:fontsize","10")
  ,("node:fontname","century schoolbook")]
  p14_edges_u

p14_gr :: [String]
p14_gr = p14_mk_gr [] p14_edges

p14_gen_tonnetz_n :: Int -> [Int] -> [Int] -> [Int]
p14_gen_tonnetz_n n k x =
  let gen_neighbours_n l z = map (+ z) l ++ map (z -) l
  in if n == 0
     then x
     else let r = nub (x ++ concatMap (gen_neighbours_n k) x)
          in p14_gen_tonnetz_n (n - 1) k r

p14_gen_tonnetz_e :: Int -> [Int] -> [Int] -> [((Int, Int), Int)]
p14_gen_tonnetz_e n k =
    let gen_e x y = ((min x y,max x y),abs (x - y))
        gen_e_n d_set x y = if abs (x - y) `elem` d_set then Just (gen_e x y) else Nothing
        f [p,q] = gen_e_n k p q
        f _ = error "p14_gen_tonnetz_e"
    in mapMaybe f . T.combinations 2 . p14_gen_tonnetz_n n k

-- Neo-Riemannian Tonnettz
p14_nrt_gr :: [String]
p14_nrt_gr =
  let e = p14_gen_tonnetz_e 3 [7,9,16] [48]
      o = [("node:shape","circle")
          ,("node:fontsize","10")
          ,("node:fontname","century schoolbook")
          ,("edge:len","1")]
      pp = (\(_,v) -> [("label",T.pc_pp (T.z_mod T.z12 v))],const [])
  in gen_graph o pp e

-- * P.31

p31_f_4_22 :: [Z12]
p31_f_4_22 = [0,2,4,7]

p31_e_set :: [([Z12],[Z12])]
p31_e_set = T.e_univ_select_u_edges (doi_of 3) (map sort (T.z_sro_ti_related T.z12 p31_f_4_22))

p31_gr :: [String]
p31_gr = gen_graph_ul [] set_pp p31_e_set

-- * P.114

p114_f_3_7 :: [Z12]
p114_f_3_7 = [0,2,5]

p114_mk_o :: Show t => t -> [T.Dot_Meta_Attr]
p114_mk_o el =
  [("node:shape","box")
  ,("edge:len",show el)
  ,("edge:fontsize","10")]

p114_mk_gr :: Double -> ([Z12] -> [Z12] -> Bool) -> [String]
p114_mk_gr el flt =
  let n = map sort (T.z_sro_ti_related T.z12 p114_f_3_7)
  in gen_flt_graph (p114_mk_o el) flt n

p114_f37_sc_pp :: [Z12] -> String
p114_f37_sc_pp = set_pp_tto_rel T.z12 [0,2,5]

p114_g0 :: [String]
p114_g0 =
  let mk_e flt = gen_u_edges flt (map sort (T.z_sro_ti_related T.z12 p114_f_3_7))
  in gen_graph_ul (p114_mk_o (2.5::Double)) p114_f37_sc_pp (mk_e (doi_of 2))

p114_g1 :: [String]
p114_g1 = p114_mk_gr 2.5 (doi_of 2)

p114_gr_set :: [(String,[String])]
p114_gr_set =
  [("p114.0.dot",p114_g0)
  ,("p114.1.dot",p114_g1)
  ,("p114.2.dot"
   ,let o = [("edge:len","1.25")]
    in gen_flt_graph o (loc_dif_of 1) (T.combinations 3 [1::Int .. 6]))
  ,("p114.3.dot",p114_mk_gr 1.5 (loc_dif_n_of 1))
  ,("p114.4.dot",p114_mk_gr 1.5 (loc_dif_of 1))
  ,("p114.5.dot",p114_mk_gr 1.5 (loc_dif_of 2))
  ,("p114.6.dot",p114_mk_gr 1.5 (loc_dif_in [1,2]))
  ,("p114.7.dot",p114_mk_gr 1.5 (loc_dif_in [1,2,3]))
  ,("p114.8.dot",p114_mk_gr 1.5 (min_vl_in [1,2,3]))
  ,("p114.9.dot",p114_mk_gr 2.0 (min_vl_in [1,2,3,4]))
  ]

-- * P.125

p125_gr :: [String]
p125_gr =
    let t :: [[Int]]
        t = [[p,q,r] | p <- [0 .. 11], q <- [0 .. 11], q > p, r <- [0 ..11], r > q]
        c = T.collate (zip (map sum t) t)
        with_h n = lookup n c
        ch = fromJust (liftM2 (++) (with_h 15) (with_h 16))
    in gen_graph_ul [] set_pp (T.e_univ_select_u_edges (doi_of 2) ch)

-- * P.131

p131_gr :: [String]
p131_gr =
    let c = let u = [6::Int .. 14]
            in [[p,q,r] | p <- u, q <- u, q > p, r <- u, r > q, p + q + r == 30]
    in gen_graph_ul [] set_pp (T.e_univ_select_u_edges (min_vl_of 2) c)

-- * P.148

p148_mk_gr :: ([Int] -> [Int] -> Bool) -> [String]
p148_mk_gr f =
    let mid_set_pp :: [Int] -> String
        mid_set_pp = concatMap show . take 3 . drop 1
        i_seq :: Num i => [[i]]
        i_seq = permutations [1,2,3,4]
        p_seq :: (Ord i,Num i) => [[i]]
        p_seq = sort (map (T.dx_d 0) i_seq)
    in gen_graph_ul [("edge:len","1.75")] mid_set_pp (T.e_univ_select_u_edges f p_seq)

p148_gr_set :: [(String,[String])]
p148_gr_set =
  [("p148.0.dot",p148_mk_gr (doi_of 4))
  ,("p148.1.dot",p148_mk_gr (min_vl_in [1]))
  ,("p148.2.dot",p148_mk_gr (min_vl_in [1,2]))
  ,("p148.3.dot",p148_mk_gr (p2_and (doi_of 4) (min_vl_in [1])))
  ,("p148.4.dot",p148_mk_gr (p2_and (doi_of 4) (min_vl_in [1,2])))
  ,("p148.5.dot",p148_mk_gr (loc_dif_n_of 1))
  ,("p148.6.dot",p148_mk_gr (loc_dif_of 1))
  ]

-- * P.162

-- > length p162_ch == 30
p162_ch :: [[Int]]
p162_ch =
  let n = [0::Int,1,2,3,4,5,6,7,8]
      c = T.combinations 4 n
  in filter ((== 1) . (`mod` 4) . sum) c

-- > length p162_e == 47
p162_e :: [T.Edge [Int]]
p162_e = T.e_univ_select_u_edges (doi_of 3) p162_ch

p162_gr :: [String]
p162_gr =
    let opt = [("graph:layout","neato")
              ,("edge:len","1.75")]
    in gen_graph_ul opt set_pp p162_e

-- * P.172

-- > M.size p172_nd_map == 24
p172_nd_map :: M.Map Int [Z12]
p172_nd_map =
    let nd_exp = map sort (T.z_sro_ti_related T.z12 [0,1,3,7])
    in M.fromList (zip [0..] nd_exp)

p172_nd_e_set :: [(Int,Int)]
p172_nd_e_set = T.e_univ_select_u_edges (m_doi_of p172_nd_map 0) [0..23]

p172_nd_e_set_alt :: [T.Edge Int]
p172_nd_e_set_alt = concatMap (T.e_path_to_edges . T.close 1) p172_cyc0

p172_gr :: G.Gr () ()
p172_gr = G.mkUGraph [0..23] p172_nd_e_set

p172_set_pp :: Int -> String
p172_set_pp = set_pp . m_get p172_nd_map

-- > let (c0,c1) = p172_all_cyc p172_gr
-- > (length c0,length c1) == (48,48)
p172_all_cyc :: ([[Int]], [[Int]])
p172_all_cyc =
    let (a, b) = T.firstSecond (T.g_partition p172_gr)
    in (L.observeAll (T.ug_hamiltonian_path_ml_0 a)
       ,L.observeAll (T.ug_hamiltonian_path_ml_0 b))

p172_cyc0 :: [[Int]]
p172_cyc0 = map (!! 0) [fst p172_all_cyc,snd p172_all_cyc]

p172_g1 :: [String]
p172_g1 = gen_graph_ul [("edge:len","2.0")] p172_set_pp p172_nd_e_set

p172_g2 :: [String]
p172_g2 = gen_graph_ul [] p172_set_pp p172_nd_e_set_alt

p172_g3 :: [String]
p172_g3 =
  let m_set_pp_tto_rel = set_pp_tto_rel T.z12 [0,1,3,7] . m_get p172_nd_map
  in gen_graph_ul [("node:shape","box"),("edge:len","2.0")] m_set_pp_tto_rel p172_nd_e_set

-- | 'T.Tto' T/n/.
tto_tn :: Integral t => t -> T.Tto t
tto_tn n = T.Tto (T.z_mod T.z12 n) 1 False

-- | 'Z.Tto' T/n/I.
tto_tni :: Integral t => t -> T.Tto t
tto_tni n = T.Tto (T.z_mod T.z12 n) 1 True

gen_tto_alt_seq :: Integral t => (t -> T.Tto t,t -> T.Tto t) -> Int -> t -> t -> t -> [T.Tto t]
gen_tto_alt_seq (f,g) k n m x =
    let t = map f (take k [x,x + n ..])
        i = map g (take k [x + m,x + m + n ..])
    in T.interleave t i

-- | /k/ is length of the T & I sequences, /n/ is the T & I sequence
-- interval, /m/ is the interval between the T & I sequence.
--
-- > r = ["T0 T5I T3 T8I T6 T11I T9 T2I","T1 T6I T4 T9I T7 T0I T10 T3I"]
-- > map (unwords . map T.tto_pp . gen_tni_seq 4 3 5) [0,1] == r
gen_tni_seq :: Integral t => Int -> t -> t -> t -> [T.Tto t]
gen_tni_seq = gen_tto_alt_seq (tto_tn,tto_tni)

-- > putStrLn $ unlines $ map (unwords . map Z.tto_pp) c4
p172_c4 :: [[T.Tto Int]]
p172_c4 = map (gen_tni_seq 3 4 9) [0 .. 3] ++ map (gen_tni_seq 2 6 11) [0 .. 5]

tto_seq_edges :: (Show t,Num t,Eq t) => [[T.Tto t]] -> [(String, String)]
tto_seq_edges = nub . sort . concatMap (map T.t2_sort . adj_cyc . map T.tto_pp)

p172_g4 :: [String]
p172_g4 = gen_graph_ul [("edge:len","2.0")] id (tto_seq_edges p172_c4)

p172_gr_set :: [(String,[String])]
p172_gr_set =
    [("p172.0.dot",p172_g1)
    ,("p172.1.dot",p172_g2)
    ,("p172.2.dot",p172_g3)
    ,("p172.3.dot",p172_g4)]

-- * P.177

-- > map (partition_ic 4) p_set
-- > map (partition_ic 6) p_set
partition_ic :: (Num t, Ord t, Show t) => t -> [t] -> ([t], [t])
partition_ic n p =
    case find ((== n) . i_to_ic . absdif) (combinations2 p) of
      Just (i,j) -> let q = sort [i,j] in (q,sort (p \\ q))
      Nothing -> error (show ("partition_ic",n,p))

p177_gr_set :: [(String,[String])]
p177_gr_set =
    let p_set = concatMap (T.z_sro_ti_related T.z12) [[0::Int,1,4,6],[0,1,3,7]]
    in [("p177.0.dot",gen_graph_ul [] set_pp (map (partition_ic 4) p_set))
       ,("p177.1.dot",gen_graph_ul_ty "circo" set_pp (map (partition_ic 6) p_set))
       ,("p177.2.dot"
        ,let gr_pp = T.gr_pp_label_v set_pp
             gr = T.g_from_edges (map (partition_ic 6) p_set)
         in T.fgl_to_udot [("edge:len","1.5")] gr_pp gr)]

-- * P.178

type SC = [Int]
type PCSET = [Int]

ait :: [SC]
ait = map T.sc ["4-Z15","4-Z29"]

-- | List of pcsets /s/ where /prime(p+s)=r/ and /prime(q+s)=r/.
-- /#p/ and /#q/ must be equal, and less than /#r/.
--
-- > mk_bridge (T.sc "4-Z15") [0,6] [1,7] == [[2,5],[8,11]]
-- > mk_bridge (T.sc "4-Z29") [0,6] [1,7] == [[2,11],[5,8]]
mk_bridge :: SC -> PCSET -> PCSET -> [PCSET]
mk_bridge r p q =
    let n = length r - length p
        c = T.combinations n [0..11]
        f s = T.z_forte_prime T.z12 (p ++ s) == r && T.z_forte_prime T.z12 (q ++ s) == r
    in filter f c

-- | 'concatMap' of 'mk_bridge'.
--
-- > mk_bridge_set ait [0,6] [1,7] == [[2,5],[8,11],[2,11],[5,8]]
mk_bridge_set :: [SC] -> PCSET -> PCSET -> [PCSET]
mk_bridge_set r_set p q = concatMap (\r -> mk_bridge r p q) r_set

mk_bridge_set_seq :: [SC] -> [PCSET] -> [[PCSET]]
mk_bridge_set_seq r_set k_seq =
    case k_seq of
      p:q:k_seq' -> mk_bridge_set r_set p q : mk_bridge_set_seq r_set (q : k_seq')
      _ -> []

-- > zip [0..] (mk_bridge_set_seq ait p178_i6_seq)
p178_i6_seq :: [PCSET]
p178_i6_seq = map (sort . (\n -> T.z_pcset T.z12 [n,n+6])) [0..6]

p178_ch :: [(PCSET,[PCSET],PCSET)]
p178_ch = zip3 p178_i6_seq (mk_bridge_set_seq ait p178_i6_seq) (tail p178_i6_seq)

type ID = Char

-- | Add 'ID' to vertices, the @2,11@ the is between @0,6@ and @1,7@
-- is /not/ the same @2,11@ that is between @3,9@ and @4,10@.
p178_e :: [((ID,PCSET),(ID,PCSET))]
p178_e =
    let f k (p,c,q) = map (\x -> (('.',p),(k,x))) c ++ map (\x -> ((k,x),('.',q))) c
    in concat (zipWith f ['a'..] p178_ch)

p178_gr_1 :: [String]
p178_gr_1 =
    let opt = [("node:shape","rectangle")
              ,("node:start","1362874")
              ,("edge:len","2")]
    in gen_graph_ul opt (set_pp . snd) p178_e

p178_gr_2 :: [String]
p178_gr_2 =
    let opt = [("node:shape","point")]
    in gen_graph_ul opt (const "") p178_e

-- * P.196

p196_gr :: [String]
p196_gr = gen_flt_graph [("edge:len","1.25")] (loc_dif_of 1) (T.combinations 3 [1::Int .. 6])

-- * P.201

type SET = [Int]
type E = (SET,SET)

bd_9_3_2_12 :: [SET]
bd_9_3_2_12 =
    [[0,1,2],[0,1,2],[0,3,4],[0,3,4],[0,5,6],[0,5,7],[0,6,8],[0,7,8]
    ,[1,3,5],[1,3,8],[1,4,5],[1,4,8],[1,6,7],[1,6,7]
    ,[2,3,6],[2,3,7],[2,4,6],[2,4,7],[2,5,8],[2,5,8]
    ,[3,5,6],[3,7,8]
    ,[4,5,7],[4,6,8]]

p201_mk_e :: [Int] -> [E]
p201_mk_e =
    let f n s = if n `elem` s then Just ([n],sort (n `delete` s)) else Nothing
        g n = mapMaybe (f n) bd_9_3_2_12
    in concatMap g

p201_e :: [[E]]
p201_e = map p201_mk_e [[0,3,4],[1,6,7],[2,5,8]]

p201_o :: [T.Dot_Meta_Attr]
p201_o =
  [("graph:splines","false")
  ,("node:shape","box")
  ,("edge:len","1.75")]

-- > length p201_gr_set
p201_gr_set :: [[String]]
p201_gr_set = map (gen_graph_ul p201_o set_pp) p201_e

p201_gr_join :: [String]
p201_gr_join =
    let e = zipWith e_add_id [0::Int ..] p201_e
    in gen_graph_ul p201_o (set_pp . snd) (concat e)

-- * P.205

bd_9_3_2_34 :: [SET]
bd_9_3_2_34 =
    [[0,1,2],[0,1,3],[0,2,4],[0,3,4]
    ,[0,5,6],[0,5,7],[0,6,8],[0,7,8]
    ,[1,2,5],[1,3,6],[1,4,5],[1,4,8]
    ,[1,6,7],[1,7,8],[2,3,6],[2,3,7]
    ,[2,4,7],[2,5,8],[2,6,8],[3,4,8]
    ,[3,5,7],[3,5,8],[4,5,6],[4,6,7]]

p205_mk_e :: [Int] -> [E]
p205_mk_e =
    let f n s = if n `elem` s then Just ([n],sort (n `delete` s)) else Nothing
        g n = mapMaybe (f n) bd_9_3_2_34
    in concatMap g

p205_gr :: [String]
p205_gr =
    let o = [("graph:splines","false"),("node:shape","box"),("edge:len","2.25")]
    in gen_graph_ul o set_pp (p205_mk_e [0..8])

-- * IO

-- > wr_graphs "/home/rohan/sw/hmt/data/dot/tj/oh/"
wr_graphs :: FilePath -> IO ()
wr_graphs dir = do
  let f (nm,gr) = writeFile (dir ++ "tj_oh_" ++ nm) (unlines gr)
  f ("p012.1.dot",p12_c5_gr)
  f ("p012.2.dot",p12_euler_plane_gr)
  f ("p014.1.dot",p14_gr_u)
  f ("p014.2.dot",p14_gr)
  f ("p014.3.dot",p14_nrt_gr)
  f ("p031.dot",p31_gr)
  mapM_ f p114_gr_set
  f ("p125.dot",p125_gr)
  f ("p131.dot",p131_gr)
  mapM_ f p148_gr_set
  f ("p162.dot",p162_gr)
  mapM_ f p172_gr_set
  mapM_ f p177_gr_set
  f ("p178.1.dot",p178_gr_1)
  f ("p178.2.dot",p178_gr_2)
  f ("p196.dot",p196_gr)
  mapM_ f (zip ["p201.1.dot","p201.2.dot","p201.3.dot"] p201_gr_set)
  f ("p201.4.dot",p201_gr_join)
  f ("p205.dot",p205_gr)
