-- | Tom Johnson. /Other Harmony: Beyond Tonal and Atonal/. Editions 75, 2014.
module Music.Theory.Graph.Johnson_2014 where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Combinations as T {- hmt -}
import qualified Music.Theory.Graph.Dot as T {- hmt -}
import qualified Music.Theory.Graph.FGL as T {- hmt -}
import qualified Music.Theory.Key as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Pitch.Note as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}
import qualified Music.Theory.Tuning.Euler as T {- hmt -}
import qualified Music.Theory.Z.SRO as T {- hmt -}

-- * Common

type Z12 = Int

mod12 :: Integral a => a -> a
mod12 n = n `mod` 12

dif :: Num a => (a, a) -> a
dif = uncurry (-)

absdif :: Num a => (a, a) -> a
absdif = abs . dif

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

loc_dif_n_of :: (Eq t, Eq i, Num i) => i -> [t] -> [t] -> Bool
loc_dif_n_of n p q = loc_dif_n p q == n

-- > min_vl [6,11,13] [6,10,14] == 2
min_vl :: (Num a,Ord a) => [a] -> [a] -> a
min_vl p q =
    let f x = sum (map absdif (zip p x))
    in minimum (map f (permutations q))

min_vl_of :: (Num a, Ord a) => a -> [a] -> [a] -> Bool
min_vl_of n p q = min_vl p q == n

min_vl_in :: (Num a, Ord a) => [a] -> [a] -> [a] -> Bool
min_vl_in n p q = min_vl p q `elem` n

set_pp :: Show t => [t] -> String
set_pp = intercalate "," . map show

-- * Graph

-- | Apply predicate to universe of possible edges.
gen_edges :: (t -> t -> Bool) -> [t] -> [T.EDGE t]
gen_edges f l = [(p,q) | p <- l, q <- l, f p q]

gen_u_edges :: Ord t => (t -> t -> Bool) -> [t] -> [T.EDGE t]
gen_u_edges f = let g p q = p < q && f p q in gen_edges g

gen_graph_ul :: Ord v => [T.DOT_ATTR] -> (v -> String) -> [T.EDGE v] -> [String]
gen_graph_ul opt pp es = T.g_to_udot opt (T.gr_pp_lift_node_f pp) (T.g_from_edges es)

gen_flt_graph :: (Ord t, Show t) => [T.DOT_ATTR] -> ([t] -> [t] -> Bool) -> [[t]] -> [String]
gen_flt_graph o f p = gen_graph_ul o set_pp (gen_u_edges f p)

-- * P.12

-- | <http://localhost/rd/?t=j&e=2016-04-04.md>
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

p14_edges :: [(T.Key,T.Key)]
p14_edges =
    let univ = [0..11]
        trs n = map (mod12 . (+ n))
        e_par = zip univ univ
        e_rel = zip univ (trs 9 univ)
        e_med = zip univ (trs 4 univ)
        del_par = [10]
        del_rel = [5,6]
        del_med = [2,5,8,11]
        rem_set r = filter (\(lhs,_) -> lhs `notElem` r)
        pc_to_key m pc = let Just (n,a) = T.pc_to_note_alteration_ks pc in (n,a,m)
        e_lift (lhs,rhs) = (pc_to_key T.Major_Mode lhs,pc_to_key T.Minor_Mode rhs)
        e_mod = concat [rem_set del_par e_par,rem_set del_rel e_rel,rem_set del_med e_med]
    in map e_lift e_mod

p14_gr :: [String]
p14_gr =
    let opt = [("graph:start","168732")]
        pp = T.gr_pp_lift_node_f T.key_lc_uc_pp
        gr = T.g_from_edges p14_edges
    in T.g_to_udot opt pp gr

-- * P.31

p31_f_4_22 :: [Z12]
p31_f_4_22 = [0,2,4,7]

p31_e_set :: [([Z12],[Z12])]
p31_e_set = gen_u_edges (doi_of 3) (map sort (T.z_sro_ti_related mod12 p31_f_4_22))

p31_gr :: [String]
p31_gr = gen_graph_ul [] set_pp p31_e_set

-- * P.114

p114_f_3_7 :: [Z12]
p114_f_3_7 = [0,2,5]

p114_mk_gr :: Show t => t -> ([Z12] -> [Z12] -> Bool) -> [String]
p114_mk_gr el flt =
    let o = [("node:shape","box")
            ,("edge:len",show el)]
    in gen_flt_graph o flt (map sort (T.z_sro_ti_related mod12 p114_f_3_7))

p114_gr_set :: [(String,[String])]
p114_gr_set =
  [("p114.1.dot",p114_mk_gr 2.5 (doi_of 2))
  ,("p114.2.dot"
   ,let o = [("edge:len","1.25")]
    in gen_flt_graph o (loc_dif_of 1) (T.combinations 3 [1..6]))
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
    let t = [[p,q,r] | p <- [0 .. 11], q <- [0 .. 11], r <- [0 ..11], q > p, r > q]
        c = T.collate (zip (map sum t) t)
        with_h n = lookup n c
        ch = fromJust (liftM2 (++) (with_h 15) (with_h 16))
    in gen_graph_ul [] set_pp (gen_u_edges (doi_of 2) ch)

-- * P.131

p131_gr :: [String]
p131_gr =
    let c = let u = [6..14]
            in [[p,q,r] | p <- u, q <- u, r <- u, q > p, r > q, p + q + r == 30]
    in gen_graph_ul [] set_pp (gen_u_edges (min_vl_of 2) c)

-- * P.148

p148_mk_gr :: ([Int] -> [Int] -> Bool) -> [String]
p148_mk_gr f =
    let mid_set_pp :: [Int] -> String
        mid_set_pp = concatMap show . take 3 . drop 1
        i_seq :: Num i => [[i]]
        i_seq = permutations [1,2,3,4]
        p_seq :: (Ord i,Num i) => [[i]]
        p_seq = sort (map (T.dx_d 0) i_seq)
    in gen_graph_ul [("edge:len","1.75")] mid_set_pp (gen_u_edges f p_seq)

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

-- * IO

wr_graphs :: IO ()
wr_graphs = do
  let f (nm,gr) = writeFile ("/home/rohan/sw/hmt/data/dot/tj_oh_" ++ nm) (unlines gr)
  f ("p012.dot",p12_euler_plane_gr)
  f ("p014.dot",p14_gr)
  f ("p031.dot",p31_gr)
  mapM_ f p114_gr_set
  f ("p125.dot",p125_gr)
  f ("p131.dot",p131_gr)
  mapM_ f p148_gr_set
