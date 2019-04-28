-- | Haskell implementations of @pct@ operations.
-- See <http://rd.slavepianos.org/?t=pct>
module Music.Theory.Z.Drape_1999 where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import Music.Theory.List
import Music.Theory.Set.List
import Music.Theory.Tuple
import Music.Theory.Z
import Music.Theory.Z.Forte_1973
import Music.Theory.Z.SRO
import Music.Theory.Z.TTO

-- | Cardinality filter
--
-- > cf [0,3] (cg [1..4]) == [[1,2,3],[1,2,4],[1,3,4],[2,3,4],[]]
cf :: (Integral n) => [n] -> [[a]] -> [[a]]
cf ns = filter (\p -> genericLength p `elem` ns)

-- | Combinatorial sets formed by considering each set as possible
-- values for slot.
--
-- > cgg [[0,1],[5,7],[3]] == [[0,5,3],[0,7,3],[1,5,3],[1,7,3]]
-- > let n = "01" in cgg [n,n,n] == ["000","001","010","011","100","101","110","111"]
cgg :: [[a]] -> [[a]]
cgg l =
    case l of
      x:xs -> [ y:z | y <- x, z <- cgg xs ]
      _ -> [[]]

-- | Combinations generator, ie. synonym for 'powerset'.
--
-- > sort (cg [0,1,3]) == [[],[0],[0,1],[0,1,3],[0,3],[1],[1,3],[3]]
cg :: [a] -> [[a]]
cg = powerset

-- | Powerset filtered by cardinality.
--
-- >>> pct cg -r3 0159
-- 015
-- 019
-- 059
-- 159
--
-- > cg_r 3 [0,1,5,9] == [[0,1,5],[0,1,9],[0,5,9],[1,5,9]]
cg_r :: (Integral n) => n -> [a] -> [[a]]
cg_r n = cf [n] . cg

{- | Chain pcsegs.

>>> echo 024579 | pct chn T0 3 | sort -u
579468 (RT8M)
579A02 (T5)

> chn_t0 z12 3 [0,2,4,5,7,9] == [[5,7,9,10,0,2],[5,7,9,4,6,8]]

>>> echo 02457t | pct chn T0 2
7A0135 (RT5I)
7A81B9 (RT9MI)

> chn_t0 z12 2 [0,2,4,5,7,10] == [[7,10,0,1,3,5],[7,10,8,1,11,9]]

-}
chn_t0 :: Integral i => Z i -> Int -> [i] -> [[i]]
chn_t0 z n p =
    let f q = take_right n p == take n q
    in filter f (z_sro_rtmi_related z p)

{- | Cyclic interval segment.

>>> echo 014295e38t76 | pct cisg
13A7864529B6

> ciseg z12 [0,1,4,2,9,5,11,3,8,10,7,6] == [1,3,10,7,8,6,4,5,2,9,11,6]

-}
ciseg :: Integral i => Z i -> [i] -> [i]
ciseg z = d_dx_by (z_sub z) . cyc

-- | Synonynm for 'z_complement'.
--
-- >>> pct cmpl 02468t
-- 13579B
--
-- > cmpl z12 [0,2,4,6,8,10] == [1,3,5,7,9,11]
cmpl :: Integral i => Z i -> [i] -> [i]
cmpl = z_complement

-- | Form cycle.
--
-- >>> echo 056 | pct cyc
-- 0560
--
-- > cyc [0,5,6] == [0,5,6,0]
cyc :: [a] -> [a]
cyc l =
    case l of
      [] -> []
      x:xs -> (x:xs) ++ [x]

-- | Diatonic set name. 'd' for diatonic set, 'm' for melodic minor
-- set, 'o' for octotonic set.
d_nm :: (Integral a) => [a] -> Maybe Char
d_nm x =
    case x of
      [0,2,4,5,7,9,11] -> Just 'd'
      [0,2,3,5,7,9,11] -> Just 'm'
      [0,1,3,4,6,7,9,10] -> Just 'o'
      _ -> Nothing

-- | Diatonic implications.
dim :: Integral i => [i] -> [(i,[i])]
dim p =
    let g (i,q) = is_subset p (z_tto_tn z12 i q)
        f = filter g . zip [0..11] . repeat
        d = [0,2,4,5,7,9,11]
        m = [0,2,3,5,7,9,11]
        o = [0,1,3,4,6,7,9,10]
    in f d ++ f m ++ f o

-- | Variant of 'dim' that is closer to the 'pct' form.
--
-- >>> pct dim 016
-- T1d
-- T1m
-- T0o
--
-- > dim_nm [0,1,6] == [(1,'d'),(1,'m'),(0,'o')]
dim_nm :: Integral i => [i] -> [(i,Char)]
dim_nm =
    let pk f (i,j) = (i,f j)
    in nubBy ((==) `on` snd) .
       map (pk (fromMaybe (error "dim_mn") . d_nm)) .
       dim

-- | Diatonic interval set to interval set.
--
-- >>> pct dis 24
-- 1256
--
-- > dis [2,4] == [1,2,5,6]
dis :: (Integral t) => [Int] -> [t]
dis =
    let is = [[], [], [1,2], [3,4], [5,6], [6,7], [8,9], [10,11]]
    in concatMap (\j -> is !! j)

-- | Degree of intersection.
--
-- >>> echo 024579e | pct doi 6 | sort -u
-- 024579A
-- 024679B
--
-- > let p = [0,2,4,5,7,9,11]
-- > doi z12 6 p p == [[0,2,4,5,7,9,10],[0,2,4,6,7,9,11]]
--
-- >>> echo 01234 | pct doi 2 7-35 | sort -u
-- 13568AB
--
-- > doi z12 2 (sc "7-35") [0,1,2,3,4] == [[1,3,5,6,8,10,11]]
doi :: Integral i => Z i -> Int -> [i] -> [i] -> [[i]]
doi z n p q =
    let f j = [z_tto_tn z j p,z_tto_tni z j p]
        xs = concatMap f [0 .. z_modulus z - 1]
    in set (filter (\x -> length (x `intersect` q) == n) xs)

-- | Embedded segment search.
--
-- >>> echo 23A | pct ess 0164325
-- 2B013A9
-- 923507A
--
-- > ess z12 [0,1,6,4,3,2,5] [2,3,10] == [[9,2,3,5,0,7,10],[2,11,0,1,3,10,9]]
ess :: Integral i => Z i -> [i] -> [i] -> [[i]]
ess z p q = filter (`is_embedding` q) (z_sro_rtmi_related z p)

-- | Forte name (ie 'sc_name').
fn :: Integral i => Z i -> [i] -> String
fn = sc_name

-- | Z-12 cycles.
frg_cyc :: Integral i => T6 [[i]]
frg_cyc =
    let add = z_add z12
        mul = z_mul z12
        c1 = [[0 .. 11]]
        c2 = map (\n -> map (add n) [0,2..10]) [0..1]
        c3 = map (\n -> map (add n) [0,3..9]) [0..2]
        c4 = map (\n -> map (add n) [0,4..8]) [0..3]
        c5 = map (map (mul 5)) c1
        c6 = map (\n -> map (add n) [0,6]) [0..5]
    in (c1,c2,c3,c4,c5,c6)

-- | Fragmentation of cycles.
frg :: Integral i =>  [i] -> T6 [String]
frg p =
    let f = map (\n -> if n `elem` p then z16_to_char n else '-')
    in t6_map (map f) frg_cyc

-- | Header sequence for 'frg_pp'.
frg_hdr :: [String]
frg_hdr = map (\n -> "Fragmentation of " ++ show n ++ "-cycle(s)") [1::Int .. 6]

{-| Fragmentation of cycles.

>>> pct frg 024579
Fragmentation of 1-cycle(s):  [0-2-45-7-9--]
Fragmentation of 2-cycle(s):  [024---] [--579-]
Fragmentation of 3-cycle(s):  [0--9] [-47-] [25--]
Fragmentation of 4-cycle(s):  [04-] [-59] [2--] [-7-]
Fragmentation of 5-cycle(s):  [05------4927]
Fragmentation of 6-cycle(s):  [0-] [-7] [2-] [-9] [4-] [5-]
IC cycle vector: <1> <22> <111> <1100> <5> <000000>

> putStrLn $ frg_pp [0,2,4,5,7,9]
-}
frg_pp :: Integral i => [i] -> String
frg_pp =
    let f = unwords . map (\p -> bracket ('[',']') p)
        g x y = x ++ ": " ++ y
    in unlines . zipWith g frg_hdr . t6_to_list . t6_map f . frg

-- | Can the set-class q (under prime form algorithm pf) be drawn from the pcset p.
has_sc_pf :: (Integral a) => ([a] -> [a]) -> [a] -> [a] -> Bool
has_sc_pf pf p q =
    let n = length q
    in pf q `elem` map pf (cf [n] (cg p))

-- | 'has_sc_pf' of 'forte_prime'
--
-- > let d = [0,2,4,5,7,9,11]
-- > has_sc z12 d (z_complement z12 d) == True
--
-- > has_sc z12 [] [] == True
has_sc :: Integral i => Z i -> [i] -> [i] -> Bool
has_sc z = has_sc_pf (forte_prime z)

-- | Interval-class cycle vector.
ic_cycle_vector :: Integral i => [i] -> T6 [Int]
ic_cycle_vector p =
    let f str = let str' = if length str > 2 then close str else str
                in length (filter (\(x,y) -> x /= '-' && y /= '-') (adj2 1 str'))
    in t6_map (map f) (frg p)

-- | Pretty printer for 'ic_cycle_vector'.
--
-- > let r = "IC cycle vector: <1> <22> <111> <1100> <5> <000000>"
-- > ic_cycle_vector_pp (ic_cycle_vector [0,2,4,5,7,9]) == r
ic_cycle_vector_pp :: T6 [Int] -> String
ic_cycle_vector_pp = ("IC cycle vector: " ++) . unwords . t6_to_list . t6_map z16_seq_pp

-- | Interval cycle filter.
--
-- >>> echo 22341 | pct icf
-- 22341
--
-- > icf [[2,2,3,4,1]] == [[2,2,3,4,1]]
icf :: (Num a,Eq a) => [[a]] -> [[a]]
icf = filter ((== 12) . sum)

-- | Interval class set to interval sets.
--
-- >>> pct ici -c 123
-- 123
-- 129
-- 1A3
-- 1A9
--
-- > ici_c [1,2,3] == [[1,2,3],[1,2,9],[1,10,3],[1,10,9]]
ici :: (Num t) => [Int] -> [[t]]
ici xs =
    let is j = [[0], [1,11], [2,10], [3,9], [4,8], [5,7], [6]] !! j
        ys = map is xs
    in cgg ys

-- | Interval class set to interval sets, concise variant.
--
-- > ici_c [1,2,3] == [[1,2,3],[1,2,9],[1,10,3],[1,10,9]]
ici_c :: [Int] -> [[Int]]
ici_c [] = []
ici_c (x:xs) = map (x:) (ici xs)

-- | Interval segment (INT).
iseg :: Integral i => Z i -> [i] -> [i]
iseg z = d_dx_by (z_sub z)

-- | Imbrications.
--
-- > let r = [[[0,2,4],[2,4,5],[4,5,7],[5,7,9]]
-- >         ,[[0,2,4,5],[2,4,5,7],[4,5,7,9]]]
-- > in imb [3,4] [0,2,4,5,7,9] == r
imb :: (Integral n) => [n] -> [a] -> [[[a]]]
imb cs p =
    let g n = (== n) . genericLength
        f ps n = filter (g n) (map (genericTake n) ps)
    in map (f (tails p)) cs

{- | 'issb' gives the set-classes that can append to 'p' to give 'q'.

>>> pct issb 3-7 6-32
3-7
3-2
3-11

> issb (Z12.sc "3-7") (Z12.sc "6-32") == ["3-2","3-7","3-11"]

-}
issb :: Integral i => Z i -> [i] -> [i] -> [String]
issb z p q =
    let k = length q - length p
        f = any id . map (\x -> forte_prime z (p ++ x) == q) . z_tto_ti_related z
    in map (sc_name z) (filter f (cf [k] scs))

-- | Matrix search.
--
-- >>> pct mxs 024579 642 | sort -u
-- 6421B9
-- B97642
--
-- > set (mxs z12 [0,2,4,5,7,9] [6,4,2]) == [[6,4,2,1,11,9],[11,9,7,6,4,2]]
mxs :: Integral i => Z i -> [i] -> [i] -> [[i]]
mxs z p q = filter (q `isInfixOf`) (z_sro_rti_related z p)

-- | Normalize (synonym for 'set')
--
-- >>> pct nrm 0123456543210
-- 0123456
--
-- > nrm [0,1,2,3,4,5,6,5,4,3,2,1,0] == [0,1,2,3,4,5,6]
nrm :: (Ord a) => [a] -> [a]
nrm = set

-- | Normalize, retain duplicate elements.
nrm_r :: (Ord a) => [a] -> [a]
nrm_r = sort

{- | Pitch-class invariances (called @pi@ at @pct@).

>>> pct pi 0236 12
pcseg 0236
pcseg 6320
pcseg 532B
pcseg B235

> pci z12 [1,2] [0,2,3,6] == [[0,2,3,6],[5,3,2,11],[6,3,2,0],[11,2,3,5]]

-}
pci :: Integral i => Z i-> [Int] -> [i] -> [[i]]
pci z i p =
    let f q = set (map (q !!) i)
    in filter (\q -> f q == f p) (z_sro_rti_related z p)

{- | Relate sets (TnMI), ie 'z_tto_rel'

>>> $ pct rs 0123 641B
>>> T1M

> map tto_pp (rs 5 z12 [0,1,2,3] [6,4,1,11]) == ["T1M","T4MI"]

-}
rs :: Integral t => t -> Z t -> [t] -> [t] -> [TTO t]
rs = z_tto_rel

{- | Relate segments.

>>> $ pct rsg 156 3BA
>>> T4I
>>> $ pct rsg 0123 05A3
>>> T0M
>>> $ pct rsg 0123 4B61
>>> RT1M
>>> $ pct rsg 0123 B614
>>> r3RT1M

> let sros = map sro_parse . words
> rsg 5 z12 [1,5,6] [3,11,10] == sros "T4I r1RT4MI"
> rsg 5 z12 [0,1,2,3] [0,5,10,3] == sros "T0M RT3MI"
> rsg 5 z12 [0,1,2,3] [4,11,6,1] == sros "T4MI RT1M"
> rsg 5 z12 [0,1,2,3] [11,6,1,4] == sros "r1T4MI r1RT1M"

-}
rsg :: Integral i => i -> Z i -> [i] -> [i] -> [SRO i]
rsg m z x y = filter (\o -> z_sro_apply m z o x == y) (z_sro_univ (length x) z)

-- | Subsets.
sb :: Integral i => Z i -> [[i]] -> [[i]]
sb z xs =
    let f p = all id (map (\q -> has_sc z p q) xs)
    in filter f scs

{- | scc = set class completion

>>> pct scc 6-32 168
35A
49B
3AB
34B

> scc z12 (sc "6-32") [1,6,8] == [[3,5,10],[4,9,11],[3,10,11],[3,4,11]]

-}
scc :: Integral i => Z i -> [i] -> [i] -> [[i]]
scc z r p = map (\\ p) (filter (is_subset p) (z_tto_ti_related z r))

-- | Header fields for 'si'.
si_hdr :: [String]
si_hdr =
    ["pitch-class-set"
    ,"set-class"
    ,"interval-class-vector"
    ,"tics"
    ,"complement"
    ,"multiplication-by-five-transform"]

-- | (PCSET,TTO,FORTE-PRIME)
type SI i = ([i],TTO i,[i])

-- | Calculator for si.
--
-- > si_calc z12 [0,5,3,11]
si_calc :: Integral i => Z i -> [i] -> (SI i,[i],[Int],SI i,SI i)
si_calc z p =
    let n = length p
        p_icv = fromIntegral n : icv z p
        gen_si x = let x_f = forte_prime z x
                       x_o:_ = rs 5 z x_f x
                   in (nub (sort x),x_o,x_f)
    in (gen_si p,p_icv,tics z p,gen_si (z_complement z p),gen_si (map (z_mul z 5) p))

-- | Pretty printer for RHS for si.
--
-- > si_rhs_pp z12 [0,5,3,11]
si_rhs_pp :: (Integral i,Show i) => Z i -> [i] -> [String]
si_rhs_pp z p =
    let pf_pp concise (x_o,x_f) =
            concat [tto_pp x_o," ",sc_name z x_f
                   ,if concise then "" else z16_vec_pp x_f]
        si_pp (x,x_o,x_f) = concat [z16_set_pp x," (",pf_pp True (x_o,x_f),")"]
        ((p',p_o,p_f),p_icv,p_tics,c,m) = si_calc z p
    in [z16_set_pp p'
       ,pf_pp False (p_o,p_f)
       ,z16_vec_pp p_icv
       ,z16_vec_pp p_tics
       ,si_pp c
       ,si_pp m]

{- | Set information.

$ pct si 053b
pitch-class-set: {035B}
set-class: TB  4-Z15[0146]
interval-class-vector: [4111111]
tics: [102222102022]
complement: {1246789A} (TAI 8-Z15)
multiplication-by-five-transform: {0317} (T0  4-Z29)
$

> putStr $ unlines $ si z12 [0,2,4,5,7,9,11] --[0,5,3,11]
-}
si :: (Integral i,Show i) => Z i -> [i] -> [String]
si z p = zipWith (\k v -> concat [k,": ",v]) si_hdr (si_rhs_pp z p)

{- | Super set-class.

>>> pct spsc 4-11 4-12
5-26[02458]

> spsc z12 [sc "4-11",sc "4-12"] == [[0,2,4,5,8]]

>>> pct spsc 3-11 3-8
4-27[0258]
4-Z29[0137]

> spsc z12 [sc "3-11",sc "3-8"] == [[0,2,5,8],[0,1,3,7]]

>>> pct spsc `pct fl 3`
6-Z17[012478]

> spsc z12 (cf [3] scs) == [[0,1,2,4,7,8]]

-}
spsc :: Integral i => Z i -> [[i]] -> [[i]]
spsc z xs =
    let f y = all (has_sc z y) xs
        g = (==) `on` length
    in (head . groupBy g . filter f) scs

{- | sra = stravinsky rotational array

>>> echo 019BA7 | pct sra
019BA7
08A96B
021A34
0B812A
0923B1
056243

> let r = [[0,1,9,11,10,7],[0,8,10,9,6,11],[0,2,1,10,3,4],[0,11,8,1,2,10],[0,9,2,3,11,1],[0,5,6,2,4,3]]
> sra z12 [0,1,9,11,10,7] == r

-}
sra :: Integral i => Z i -> [i] -> [[i]]
sra z = map (z_sro_tn_to z 0) . rotations

{- | Serial operation.

>>> echo 156 | pct sro T4
59A

> sro (Z.sro_parse "T4") [1,5,6] == [5,9,10]

>>> echo 024579 | pct sro RT4I
79B024

> sro (Z.SRO 0 True 4 False True) [0,2,4,5,7,9] == [7,9,11,0,2,4]

>>> echo 156 | pct sro T4I
3BA

> sro (Z.sro_parse "T4I") [1,5,6] == [3,11,10]
> sro (Z.SRO 0 False 4 False True) [1,5,6] == [3,11,10]

>>> echo 156 | pct sro T4  | pct sro T0I
732

> (sro (Z.sro_parse "T0I") . sro (Z.sro_parse "T4")) [1,5,6] == [7,3,2]

>>> echo 024579 | pct sro RT4I
79B024

> sro (Z.sro_parse "RT4I") [0,2,4,5,7,9] == [7,9,11,0,2,4]

-}
sro :: Integral i => Z i -> SRO i -> [i] -> [i]
sro z o = z_sro_apply 5 z o

{- | tmatrix

>>> pct tmatrix 1258

1258
0147
9A14
67A1

> tmatrix z12 [1,2,5,8] == [[1,2,5,8],[0,1,4,7],[9,10,1,4],[6,7,10,1]]

-}
tmatrix :: Integral i => Z i -> [i] -> [[i]]
tmatrix z p =
    let i = map (z_negate z) (d_dx_by (z_sub z) p)
    in map (\n -> map (z_add z n) p) (dx_d 0 i)


{- | trs = transformations search.  Search all RTnMI of /p/ for /q/.

>>> echo 642 | pct trs 024579 | sort -u
531642
6421B9
642753
B97642

> let r = [[5,3,1,6,4,2],[6,4,2,1,11,9],[6,4,2,7,5,3],[11,9,7,6,4,2]]
> sort (trs z12 [0,2,4,5,7,9] [6,4,2]) == r

-}
trs :: Integral i => Z i -> [i] -> [i] -> [[i]]
trs z p q = filter (q `isInfixOf`) (z_sro_rtmi_related z p)

-- | Like 'trs', but of 'z_sro_rti_related'.
--
-- > trs_m z12 [0,2,4,5,7,9] [6,4,2] == [[6,4,2,1,11,9],[11,9,7,6,4,2]]
trs_m :: Integral i => Z i -> [i] -> [i] -> [[i]]
trs_m z p q = filter (q `isInfixOf`) (z_sro_rti_related z p)
