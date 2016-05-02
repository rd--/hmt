-- | Haskell implementations of @pct@ operations.
-- See <http://slavepianos.org/rd/t/pct>.
module Music.Theory.Z12.Drape_1999 where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Safe {- safe -}

import qualified Music.Theory.List as T
import qualified Music.Theory.Set.List as T

import qualified Music.Theory.Z as Z
import qualified Music.Theory.Z.SRO as Z
import qualified Music.Theory.Z.TTO as Z

import Music.Theory.Z12
import qualified Music.Theory.Z12.Forte_1973 as Z12
import qualified Music.Theory.Z12.TTO as Z12
import qualified Music.Theory.Z12.SRO as Z12

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

-- | Combinations generator, ie. synonym for 'T.powerset'.
--
-- > sort (cg [0,1,3]) == [[],[0],[0,1],[0,1,3],[0,3],[1],[1,3],[3]]
cg :: [a] -> [[a]]
cg = T.powerset

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

> chn_t0 3 [0,2,4,5,7,9] == [[5,7,9,10,0,2],[5,7,9,4,6,8]]

>>> echo 02457t | pct chn T0 2
7A0135 (RT5I)
7A81B9 (RT9MI)

> chn_t0 2 [0,2,4,5,7,10] == [[7,10,0,1,3,5],[7,10,8,1,11,9]]

-}
chn_t0 :: Int -> [Z12] -> [[Z12]]
chn_t0 n p =
    let f q = T.take_right n p == take n q
    in filter f (Z12.sro_rtmi_related p)

{- | Cyclic interval segment.

>>> echo 014295e38t76 | pct cisg
13A7864529B6

> ciseg [0,1,4,2,9,5,11,3,8,10,7,6] == [1,3,10,7,8,6,4,5,2,9,11,6]

-}
ciseg :: [Z12] -> [Z12]
ciseg = T.d_dx . cyc

-- | Synonynm for 'complement'.
--
-- >>> pct cmpl 02468t
-- 13579B
--
-- > cmpl [0,2,4,6,8,10] == [1,3,5,7,9,11]
cmpl :: [Z12] -> [Z12]
cmpl = complement

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
dim :: [Z12] -> [(Z12,[Z12])]
dim p =
    let g (i,q) = T.is_subset p (Z12.tto_tn i q)
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
dim_nm :: [Z12] -> [(Z12,Char)]
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
-- > in doi 6 p p == [[0,2,4,5,7,9,10],[0,2,4,6,7,9,11]]
--
-- >>> echo 01234 | pct doi 2 7-35 | sort -u
-- 13568AB
--
-- > doi 2 (T.sc "7-35") [0,1,2,3,4] == [[1,3,5,6,8,10,11]]
doi :: Int -> [Z12] -> [Z12] -> [[Z12]]
doi n p q =
    let f j = [Z12.tto_tn j p,Z12.tto_tni j p]
        xs = concatMap f [0..11]
    in T.set (filter (\x -> length (x `intersect` q) == n) xs)

-- | Forte name.
fn :: [Z12] -> String
fn = Z12.sc_name

-- | p `has_ess` q is true iff p can embed q in sequence.
has_ess :: [Z12] -> [Z12] -> Bool
has_ess p q =
    case (p,q) of
      (_,[]) -> True
      ([],_) -> False
      (x:p',y:q') -> has_ess p' (if x == y then q' else q)

-- | Embedded segment search.
--
-- >>> echo 23A | pct ess 0164325
-- 2B013A9
-- 923507A
--
-- > ess [2,3,10] [0,1,6,4,3,2,5] == [[9,2,3,5,0,7,10],[2,11,0,1,3,10,9]]
ess :: [Z12] -> [Z12] -> [[Z12]]
ess p = filter (`has_ess` p) . Z12.sro_rtmi_related

-- | Can the set-class q (under prime form algorithm pf) be
--   drawn from the pcset p.
has_sc_pf :: (Integral a) => ([a] -> [a]) -> [a] -> [a] -> Bool
has_sc_pf pf p q =
    let n = length q
    in pf q `elem` map pf (cf [n] (cg p))

-- | Can the set-class q be drawn from the pcset p.
--
-- > let d = [0,2,4,5,7,9,11] in has_sc d (complement d) == True
-- > has_sc [] [] == True
has_sc :: [Z12] -> [Z12] -> Bool
has_sc = has_sc_pf Z12.forte_prime

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

-- | Interval-class segment.
--
-- >>> pct icseg 013265e497t8
-- 12141655232
--
-- > icseg [0,1,3,2,6,5,11,4,9,7,10,8] == [1,2,1,4,1,6,5,5,2,3,2]
icseg :: [Z12] -> [Z12]
icseg = map Z12.ic . iseg

-- | Interval segment (INT).
iseg :: [Z12] -> [Z12]
iseg = T.d_dx

-- | Imbrications.
imb :: (Integral n) => [n] -> [a] -> [[a]]
imb cs p =
    let g n = (== n) . genericLength
        f ps n = filter (g n) (map (genericTake n) ps)
    in concatMap (f (tails p)) cs

{- | 'issb' gives the set-classes that can append to 'p' to give 'q'.

>>> pct issb 3-7 6-32
3-7
3-2
3-11

> issb (T.sc "3-7") (T.sc "6-32") == ["3-2","3-7","3-11"]

-}
issb :: [Z12] -> [Z12] -> [String]
issb p q =
    let k = length q - length p
        f = any id . map (\x -> Z12.forte_prime (p ++ x) == q) . Z12.tto_ti_related
    in map Z12.sc_name (filter f (cf [k] Z12.scs))

-- | Matrix search.
--
-- >>> pct mxs 024579 642 | sort -u
-- 6421B9
-- B97642
--
-- > T.set (mxs [0,2,4,5,7,9] [6,4,2]) == [[6,4,2,1,11,9],[11,9,7,6,4,2]]
mxs :: [Z12] -> [Z12] -> [[Z12]]
mxs p q = filter (q `isInfixOf`) (Z12.sro_rti_related p)

-- | Normalize.
--
-- >>> pct nrm 0123456543210
-- 0123456
--
-- > nrm [0,1,2,3,4,5,6,5,4,3,2,1,0] == [0,1,2,3,4,5,6]
nrm :: (Ord a) => [a] -> [a]
nrm = T.set

-- | Normalize, retain duplicate elements.
nrm_r :: (Ord a) => [a] -> [a]
nrm_r = sort

-- | Pitch-class invariances (called @pi@ at @pct@).
--
-- >>> pct pi 0236 12
-- pcseg 0236
-- pcseg 6320
-- pcseg 532B
-- pcseg B235
--
-- > pci [0,2,3,6] [1,2] == [[0,2,3,6],[5,3,2,11],[6,3,2,0],[11,2,3,5]]
pci :: [Z12] -> [Z12] -> [[Z12]]
pci p i =
    let f q = T.set (map (q `genericIndex`) i)
    in filter (\q -> f q == f p) (Z12.sro_rti_related p)

-- | Relate sets (TnMI).
--
-- >>> pct rs 0123 641e
-- T1M
--
-- > rs [0,1,2,3] [6,4,1,11] == [(Z.tto_parse "T1M",[1,6,11,4])
-- >                            ,(Z.tto_parse "T4MI",[4,11,6,1])]
rs :: [Z12] -> [Z12] -> [(Z.TTO Z12, [Z12])]
rs x y =
    let xs = map (\o -> (o,Z.z_tto_apply 5 id o x)) (Z.z_tto_univ id)
        q = T.set y
    in filter (\(_,p) -> T.set p == q) xs

rs1 :: [Z12] -> [Z12] -> Maybe (Z.TTO Z12)
rs1 p = fmap fst . headMay . rs p

{- | Relate segments.

>>> pct rsg 156 3BA
T4I

> rsg [1,5,6] [3,11,10] == [Z.sro_parse "T4I",Z.sro_parse "r1RT4MI"]

>>> pct rsg 0123 05t3
T0M

> rsg [0,1,2,3] [0,5,10,3] == [Z.sro_parse "T0M",Z.sro_parse "RT3MI"]

>>> pct rsg 0123 4e61
RT1M

> rsg [0,1,2,3] [4,11,6,1] == [Z.sro_parse "T4MI",Z.sro_parse "RT1M"]

>>> echo e614 | pct rsg 0123
r3RT1M

> rsg [0,1,2,3] [11,6,1,4] == [Z.sro_parse "r1T4MI",Z.sro_parse "r1RT1M"]

-}
rsg :: [Z12] -> [Z12] -> [Z.SRO Z12]
rsg x y = filter (\o -> sro o x == y) (Z.z_sro_univ (length x) id)

-- | Subsets.
sb :: [[Z12]] -> [[Z12]]
sb xs =
    let f p = all id (map (`has_sc` p) xs)
    in filter f Z12.scs

si_hdr :: [String]
si_hdr =
    ["pitch-class-set"
    ,"set-class"
    ,"interval-class-vector"
    ,"tics"
    ,"complement"
    ,"multiplication-by-five-transform"]

type SI = ([Z12],Z.TTO Z12,[Z12])

-- > si_raw [0,5,3,11]
si_raw :: [Z12] -> (SI,[Z12],[Int],SI,SI)
si_raw p =
    let n = length p
        p_icv = to_Z12 n : Z12.icv p
        gen_si x = let x_f = Z12.forte_prime x
                       Just x_o = rs1 x_f x
                   in (nub (sort x),x_o,x_f)
    in (gen_si p,p_icv,tics p,gen_si (complement p),gen_si (map (* 5) p))

si_raw_pp :: [Z12] -> [String]
si_raw_pp p =
    let pf_pp concise (x_o,x_f) =
            concat [Z.tto_pp x_o," ",Z12.sc_name x_f
                   ,if concise then "" else z12_vec_pp x_f]
        si_pp (x,x_o,x_f) = concat [z12_set_pp x," (",pf_pp True (x_o,x_f),")"]
        ((p',p_o,p_f),p_icv,p_tics,c,m) = si_raw p
    in [z12_set_pp p'
       ,pf_pp False (p_o,p_f)
       ,z12_vec_pp p_icv
       ,Z.z16_vec_pp p_tics
       ,si_pp c
       ,si_pp m]

-- | Set information.
--
-- > putStr $ unlines $ si [0,5,3,11]
si :: [Z12] -> [String]
si p = zipWith (\k v -> concat [k,": ",v]) si_hdr (si_raw_pp p)

{- | Super set-class.

>>> pct spsc 4-11 4-12
5-26[02458]

> spsc [T.sc "4-11",T.sc "4-12"] == ["5-26"]

>>> pct spsc 3-11 3-8
4-27[0258]
4-Z29[0137]

> spsc [T.sc "3-11",T.sc "3-8"] == ["4-27","4-Z29"]

>>> pct spsc `pct fl 3`
6-Z17[012478]

> spsc (cf [3] T.scs) == ["6-Z17"]

-}
spsc :: [[Z12]] -> [String]
spsc xs =
    let f y = all (y `has_sc`) xs
        g = (==) `on` length
    in (map Z12.sc_name . head . groupBy g . filter f) Z12.scs

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
sro :: Z.SRO Z12 -> [Z12] -> [Z12]
sro o = Z.z_sro_apply 5 id o

-- | Vector indicating degree of intersection with inversion at each transposition.
--
-- > tics [0,2,4,5,7,9] == [3,2,5,0,5,2,3,4,1,6,1,4]
-- > map tics Z12.scs
tics :: [Z12] -> [Int]
tics p =
    let q = Z12.tto_t_related (Z12.tto_invert 0 p)
    in map (length . intersect p) q
