{- | Haskell implementations of @pct@ operations.
See <http://rd.slavepianos.org/?t=pct>
-}
module Music.Theory.Z.Drape_1999 where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as List {- hmt -}
import qualified Music.Theory.Set.List as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

import Music.Theory.Z
import Music.Theory.Z.Forte_1973
import Music.Theory.Z.Sro
import Music.Theory.Z.Tto

{- | Cardinality filter

> cf [0,3] (cg [1..4]) == [[1,2,3],[1,2,4],[1,3,4],[2,3,4],[]]
-}
cf :: (Integral n) => [n] -> [[a]] -> [[a]]
cf ns = filter (\p -> genericLength p `elem` ns)

{- | Combinatorial sets formed by considering each set as possible
values for slot.

> cgg [[0,1],[5,7],[3]] == [[0,5,3],[0,7,3],[1,5,3],[1,7,3]]
> let n = "01" in cgg [n,n,n] == ["000","001","010","011","100","101","110","111"]
-}
cgg :: [[a]] -> [[a]]
cgg l =
  case l of
    x : xs -> [y : z | y <- x, z <- cgg xs]
    _ -> [[]]

{- | Combinations generator, ie. synonym for 'powerset'.

> sort (cg [0,1,3]) == [[],[0],[0,1],[0,1,3],[0,3],[1],[1,3],[3]]
-}
cg :: [a] -> [[a]]
cg = T.powerset

{- | Powerset filtered by cardinality.

pct cg -r3 0159

>>> cg_r 3 [0,1,5,9]
[[0,1,5],[0,1,9],[0,5,9],[1,5,9]]
-}
cg_r :: (Integral n) => n -> [a] -> [[a]]
cg_r n = cf [n] . cg

{- | Chain pcsegs.

echo 024579 | pct chn T0 3 | sort -u

>>> chn_t0 z12 3 [0,2,4,5,7,9]
[[5,7,9,10,0,2],[5,7,9,4,6,8]]

echo 02457t | pct chn T0 2

>>> chn_t0 z12 2 [0,2,4,5,7,10]
[[7,10,0,1,3,5],[7,10,8,1,11,9]]
-}
chn_t0 :: Integral i => Z i -> Int -> [i] -> [[i]]
chn_t0 z n p =
  let f q = List.take_right n p == take n q
  in filter f (z_sro_rtmi_related z p)

{- | Cyclic interval segment.

echo 014295e38t76 | pct cisg

>>> ciseg z12 [0,1,4,2,9,5,11,3,8,10,7,6]
[1,3,10,7,8,6,4,5,2,9,11,6]
-}
ciseg :: Integral i => Z i -> [i] -> [i]
ciseg z = List.d_dx_by (z_sub z) . cyc

{- | Synonynm for 'z_complement'.

pct cmpl 02468t

>>> cmpl z12 [0,2,4,6,8,10]
[1,3,5,7,9,11]
-}
cmpl :: Integral i => Z i -> [i] -> [i]
cmpl = z_complement

{- | Form cycle.

echo 056 | pct cyc

>>> cyc [0,5,6]
[0,5,6,0]
-}
cyc :: [a] -> [a]
cyc l =
  case l of
    [] -> []
    x : xs -> (x : xs) ++ [x]

{- | Diatonic set name. 'd' for diatonic set, 'm' for melodic minor
set, 'o' for octotonic set.
-}
d_nm :: (Integral a) => [a] -> Maybe Char
d_nm x =
  case x of
    [0, 2, 4, 5, 7, 9, 11] -> Just 'd'
    [0, 2, 3, 5, 7, 9, 11] -> Just 'm'
    [0, 1, 3, 4, 6, 7, 9, 10] -> Just 'o'
    _ -> Nothing

-- | Diatonic implications.
dim :: Integral i => [i] -> [(i, [i])]
dim p =
  let g (i, q) = List.is_subset p (z_tto_tn z12 i q)
      f = filter g . zip [0 .. 11] . repeat
      d = [0, 2, 4, 5, 7, 9, 11]
      m = [0, 2, 3, 5, 7, 9, 11]
      o = [0, 1, 3, 4, 6, 7, 9, 10]
  in f d ++ f m ++ f o

{- | Variant of 'dim' that is closer to the 'pct' form.

pct dim 016

>>> dim_nm [0,1,6]
[(1,'d'),(1,'m'),(0,'o')]
-}
dim_nm :: Integral i => [i] -> [(i, Char)]
dim_nm =
  let pk f (i, j) = (i, f j)
  in nubBy ((==) `on` snd)
      . map (pk (fromMaybe (error "dim_mn") . d_nm))
      . dim

{- | Diatonic interval set to interval set.

pct dis 24

>>> dis [2,4]
[1,2,5,6]
-}
dis :: (Integral t) => [Int] -> [t]
dis =
  let is = [[], [], [1, 2], [3, 4], [5, 6], [6, 7], [8, 9], [10, 11]]
  in concatMap (is !!)

{- | Degree of intersection.

echo 024579e | pct doi 6 | sort -u

>>> let p = [0,2,4,5,7,9,11]
>>> doi z12 6 p p
[[0,2,4,5,7,9,10],[0,2,4,6,7,9,11]]

echo 01234 | pct doi 2 7-35 | sort -u

>>> doi z12 2 (sc "7-35") [0,1,2,3,4]
[[1,3,5,6,8,10,11]]
-}
doi :: Integral i => Z i -> Int -> [i] -> [i] -> [[i]]
doi z n p q =
  let f j = [z_tto_tn z j p, z_tto_tni z j p]
      xs = concatMap f [0 .. z_modulus z - 1]
  in T.set (filter (\x -> length (x `intersect` q) == n) xs)

{- | Embedded segment search.

echo 23A | pct ess 0164325

>>> ess z12 [0,1,6,4,3,2,5] [2,3,10]
[[9,2,3,5,0,7,10],[2,11,0,1,3,10,9]]
-}
ess :: Integral i => Z i -> [i] -> [i] -> [[i]]
ess z p q = filter (`List.is_embedding` q) (z_sro_rtmi_related z p)

-- | Forte name (ie 'sc_name').
fn :: Integral i => [i] -> String
fn = sc_name

-- | Z-12 cycles.
frg_cyc :: Integral i => T.T6 [[i]]
frg_cyc =
  let add = z_add z12
      mul = z_mul z12
      c1 = [[0 .. 11]]
      c2 = map (\n -> map (add n) [0, 2 .. 10]) [0 .. 1]
      c3 = map (\n -> map (add n) [0, 3 .. 9]) [0 .. 2]
      c4 = map (\n -> map (add n) [0, 4 .. 8]) [0 .. 3]
      c5 = map (map (mul 5)) c1
      c6 = map (\n -> map (add n) [0, 6]) [0 .. 5]
  in (c1, c2, c3, c4, c5, c6)

-- | Fragmentation of cycles.
frg :: Integral i => [i] -> T.T6 [String]
frg p =
  let f = map (\n -> if n `elem` p then z16_to_char n else '-')
  in T.t6_map (map f) frg_cyc

-- | Header sequence for 'frg_pp'.
frg_hdr :: [String]
frg_hdr = map (\n -> "Fragmentation of " ++ show n ++ "-cycle(s)") [1 :: Int .. 6]

{- | Fragmentation of cycles.

pct frg 024579

>>> frg_pp [0,2,4,5,7,9]
"Fragmentation of 1-cycle(s): [0-2-45-7-9--]\nFragmentation of 2-cycle(s): [024---] [--579-]\nFragmentation of 3-cycle(s): [0--9] [-47-] [25--]\nFragmentation of 4-cycle(s): [04-] [-59] [2--] [-7-]\nFragmentation of 5-cycle(s): [05------4927]\nFragmentation of 6-cycle(s): [0-] [-7] [2-] [-9] [4-] [5-]\n"
-}
frg_pp :: Integral i => [i] -> String
frg_pp =
  let f = unwords . map (List.bracket ('[', ']'))
      g x y = x ++ ": " ++ y
  in unlines . zipWith g frg_hdr . T.t6_to_list . T.t6_map f . frg

-- | Can the set-class q (under prime form algorithm pf) be drawn from the pcset p.
has_sc_pf :: (Integral a) => ([a] -> [a]) -> [a] -> [a] -> Bool
has_sc_pf pf p q =
  let n = length q
  in pf q `elem` map pf (cf [n] (cg p))

{- | 'has_sc_pf' of 'forte_prime'

> let d = [0,2,4,5,7,9,11]
> has_sc z12 d (z_complement z12 d) == True

> has_sc z12 [] [] == True
-}
has_sc :: Integral i => Z i -> [i] -> [i] -> Bool
has_sc z = has_sc_pf (z_forte_prime z)

-- | Interval-class cycle vector.
ic_cycle_vector :: Integral i => [i] -> T.T6 [Int]
ic_cycle_vector p =
  let f str =
        let str' = if length str > 2 then List.close 1 str else str
        in length (filter (\(x, y) -> x /= '-' && y /= '-') (List.adj2 1 str'))
  in T.t6_map (map f) (frg p)

{- | Pretty printer for 'ic_cycle_vector'.

> let r = "IC cycle vector: <1> <22> <111> <1100> <5> <000000>"
> ic_cycle_vector_pp (ic_cycle_vector [0,2,4,5,7,9]) == r
-}
ic_cycle_vector_pp :: T.T6 [Int] -> String
ic_cycle_vector_pp = ("IC cycle vector: " ++) . unwords . T.t6_to_list . T.t6_map z16_seq_pp

{- | Interval cycle filter.

echo 22341 | pct icf

>>> icf [[2,2,3,4,1]]
[[2,2,3,4,1]]
-}
icf :: (Num a, Eq a) => [[a]] -> [[a]]
icf = filter ((== 12) . sum)

{- | Interval class set to interval sets.

pct ici -c 123

>>> ici_c [1,2,3]
[[1,2,3],[1,2,9],[1,10,3],[1,10,9]]
-}
ici :: (Num t) => [Int] -> [[t]]
ici xs =
  let is j = [[0], [1, 11], [2, 10], [3, 9], [4, 8], [5, 7], [6]] !! j
      ys = map is xs
  in cgg ys

{- | Interval class set to interval sets, concise variant.

> ici_c [1,2,3] == [[1,2,3],[1,2,9],[1,10,3],[1,10,9]]
-}
ici_c :: [Int] -> [[Int]]
ici_c [] = []
ici_c (x : xs) = map (x :) (ici xs)

-- | Interval segment (INT).
iseg :: Integral i => Z i -> [i] -> [i]
iseg z = List.d_dx_by (z_sub z)

{- | Imbrications.

> let r = [[[0,2,4],[2,4,5],[4,5,7],[5,7,9]]
>         ,[[0,2,4,5],[2,4,5,7],[4,5,7,9]]]
> in imb [3,4] [0,2,4,5,7,9] == r
-}
imb :: (Integral n) => [n] -> [a] -> [[[a]]]
imb cs p =
  let g n = (== n) . genericLength
      f ps n = filter (g n) (map (genericTake n) ps)
  in map (f (tails p)) cs

{- | 'issb' gives the set-classes that can append to 'p' to give 'q'.

pct issb 3-7 6-32

>>> issb (sc "3-7") (sc "6-32")
["3-2","3-7","3-11"]
-}
issb :: Integral i => [i] -> [i] -> [String]
issb p q =
  let k = length q - length p
      f = any (\x -> z_forte_prime z12 (nub (p ++ x)) == q) . z_tto_ti_related z12
  in map sc_name (filter f (cf [k] scs))

{- | Matrix search.

pct mxs 024579 642 | sort -u

>>> mxs z12 [0,2,4,5,7,9] [6,4,2]
[[6,4,2,1,11,9],[11,9,7,6,4,2]]
-}
mxs :: Integral i => Z i -> [i] -> [i] -> [[i]]
mxs z p q = filter (q `isInfixOf`) (z_sro_rti_related z p)

{- | Normalize (synonym for 'set')

pct nrm 0123456543210


>>> nrm [0,1,2,3,4,5,6,5,4,3,2,1,0]
[0,1,2,3,4,5,6]
-}
nrm :: (Ord a) => [a] -> [a]
nrm = T.set

-- | Normalize, retain duplicate elements.
nrm_r :: (Ord a) => [a] -> [a]
nrm_r = sort

{- | Pitch-class invariances (called @pi@ at @pct@).

pct pi 0236 12

>>> pci z12 [1,2] [0,2,3,6]
[[0,2,3,6],[5,3,2,11],[6,3,2,0],[11,2,3,5]]
-}
pci :: Integral i => Z i -> [Int] -> [i] -> [[i]]
pci z i p =
  let f q = T.set (map (q !!) i)
  in filter (\q -> f q == f p) (z_sro_rti_related z p)

{- | Relate sets (TnMI), ie 'z_tto_rel'

pct rs 0123 641B

>>> map tto_pp (rs 5 z12 [0,1,2,3] [6,4,1,11])
["T1M","T4MI"]
-}
rs :: Integral t => t -> Z t -> [t] -> [t] -> [Tto t]
rs m z p q = z_tto_rel m z (T.set p) (T.set q)

{- | Relate segments.

pct rsg 156 3BA = T4I
pct rsg 0123 05A3 = T0M
pct rsg 0123 4B61 = RT1M
pct rsg 0123 B614 = r3RT1M

>>> let sros = map (sro_parse 5) . words
>>> rsg 5 z12 [1,5,6] [3,11,10] == sros "T4I r1RT4MI"
True

>>> rsg 5 z12 [0,1,2,3] [0,5,10,3] == sros "T0M RT3MI"
True

>>> rsg 5 z12 [0,1,2,3] [4,11,6,1] == sros "T4MI RT1M"
True

>>> rsg 5 z12 [0,1,2,3] [11,6,1,4] == sros "r1T4MI r1RT1M"
True
-}
rsg :: Integral i => i -> Z i -> [i] -> [i] -> [Sro i]
rsg = z_sro_rel

{- | Subsets.

> cf [4] (sb z12 [sc "6-32",sc "6-8"]) == [[0,2,3,5],[0,1,3,5],[0,2,3,7],[0,2,4,7],[0,2,5,7]]
-}
sb :: Integral i => Z i -> [[i]] -> [[i]]
sb z xs =
  let f p = all (\q -> has_sc z q p) xs
  in filter f scs

{- | scc = set class completion

pct scc 6-32 168

>>> scc z12 (sc "6-32") [1,6,8]
[[3,5,10],[4,9,11],[3,10,11],[3,4,11]]
-}
scc :: Integral i => Z i -> [i] -> [i] -> [[i]]
scc z r p = map (\\ p) (filter (List.is_subset p) (z_tto_ti_related z r))

-- | Header fields for 'si'.
si_hdr :: [String]
si_hdr =
  [ "pitch-class-set"
  , "set-class"
  , "interval-class-vector"
  , "tics"
  , "complement"
  , "multiplication-by-five-transform"
  ]

-- | (Pcset,Tto,Forte-Prime)
type Si i = ([i], Tto i, [i])

{- | Calculator for si.

> si_calc [0,5,3,11]
-}
si_calc :: Integral i => [i] -> (Si i, [i], [Int], Si i, Si i)
si_calc p =
  let n = length p
      p_icv = fromIntegral n : z_icv z12 p
      gen_si x =
        let x_f = z_forte_prime z12 x
            x_o = List.head_err (rs 5 z12 x_f x)
        in (nub (sort x), x_o, x_f)
  in (gen_si p, p_icv, tics z12 p, gen_si (z_complement z12 p), gen_si (map (z_mul z12 5) p))

{- | Pretty printer for RHS for si.

> si_rhs_pp [0,5,3,11]
-}
si_rhs_pp :: (Integral i, Show i) => [i] -> [String]
si_rhs_pp p =
  let pf_pp concise (x_o, x_f) =
        concat
          [ tto_pp x_o
          , " "
          , sc_name x_f
          , if concise then "" else z16_vec_pp x_f
          ]
      si_pp (x, x_o, x_f) = concat [z16_set_pp x, " (", pf_pp True (x_o, x_f), ")"]
      ((p', p_o, p_f), p_icv, p_tics, c, m) = si_calc p
  in [ z16_set_pp p'
     , pf_pp False (p_o, p_f)
     , z16_vec_pp p_icv
     , z16_vec_pp p_tics
     , si_pp c
     , si_pp m
     ]

{- | Set information.

\$ pct si 053b
pitch-class-set: {035B}
set-class: TB  4-Z15[0146]
interval-class-vector: [4111111]
tics: [102222102022]
complement: {1246789A} (TAI 8-Z15)
multiplication-by-five-transform: {0317} (T0  4-Z29)
\$

> putStr $ unlines $ si [0,5,3,11]
-}
si :: (Integral i, Show i) => [i] -> [String]
si p = zipWith (\k v -> concat [k, ": ", v]) si_hdr (si_rhs_pp p)

{- | Super set-class.

pct spsc 4-11 4-12 = 5-26[02458]

>>> spsc z12 [sc "4-11",sc "4-12"]
[[0,2,4,5,8]]

pct spsc 3-11 3-8

>>> spsc z12 [sc "3-11",sc "3-8"]
[[0,2,5,8],[0,1,3,7]]

pct spsc `pct fl 3` = 6-Z17[012478]

>>> spsc z12 (cf [3] scs)
[[0,1,2,4,7,8]]
-}
spsc :: Integral i => Z i -> [[i]] -> [[i]]
spsc z xs =
  let f y = all (has_sc z y) xs
      g = (==) `on` length
  in (List.head_err . groupBy g . filter f) scs

{- | sra = stravinsky rotational array

echo 019BA7 | pct sra

>>> sra z12 [0,1,9,11,10,7]
[[0,1,9,11,10,7],[0,8,10,9,6,11],[0,2,1,10,3,4],[0,11,8,1,2,10],[0,9,2,3,11,1],[0,5,6,2,4,3]]
-}
sra :: Integral i => Z i -> [i] -> [[i]]
sra z = map (z_sro_tn_to z 0) . List.rotations

{- | Serial operation.

echo 156 | pct sro T4 = 59A

>>> sro z12 (sro_parse 5 "T4") [1,5,6]
[5,9,10]

echo 024579 | pct sro RT4I = 79B024

>>> sro z12 (Sro 0 True 4 1 True) [0,2,4,5,7,9]
[7,9,11,0,2,4]

echo 156 | pct sro T4I = 3BA

>>> sro z12 (sro_parse 5 "T4I") [1,5,6]
[3,11,10]

>>> sro z12 (Sro 0 False 4 1 True) [1,5,6]
[3,11,10]

echo 156 | pct sro T4  | pct sro T0I = 732

>>> (sro z12 (sro_parse 5 "T0I") . sro z12 (sro_parse 5 "T4")) [1,5,6]
[7,3,2]

echo 024579 | pct sro RT4I = 79B024

>>> sro z12 (sro_parse 5 "RT4I") [0,2,4,5,7,9]
[7,9,11,0,2,4]
-}
sro :: Integral i => Z i -> Sro i -> [i] -> [i]
sro = z_sro_apply

{- | tmatrix

pct tmatrix 1258

>>> tmatrix z12 [1,2,5,8]
[[1,2,5,8],[0,1,4,7],[9,10,1,4],[6,7,10,1]]
-}
tmatrix :: Integral i => Z i -> [i] -> [[i]]
tmatrix z p =
  let i = map (z_negate z) (List.d_dx_by (z_sub z) p)
  in map (\n -> map (z_add z n) p) (List.dx_d 0 i)

{- | trs = transformations search.  Search all RTnMI of /p/ for /q/.

echo 642 | pct trs 024579 | sort -u

>>> sort (trs z12 [0,2,4,5,7,9] [6,4,2])
[[5,3,1,6,4,2],[6,4,2,1,11,9],[6,4,2,7,5,3],[11,9,7,6,4,2]]
-}
trs :: Integral i => Z i -> [i] -> [i] -> [[i]]
trs z p q = filter (q `isInfixOf`) (z_sro_rtmi_related z p)

{- | Like 'trs', but of 'z_sro_rti_related'.

> trs_m z12 [0,2,4,5,7,9] [6,4,2] == [[6,4,2,1,11,9],[11,9,7,6,4,2]]
-}
trs_m :: Integral i => Z i -> [i] -> [i] -> [[i]]
trs_m z p q = filter (q `isInfixOf`) (z_sro_rti_related z p)
