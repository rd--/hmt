-- | Haskell implementations of @pct@ operations.
-- See <http://slavepianos.org/rd/t/pct>.
module Music.Theory.Z12.Drape_1999 where

import Data.List {- base -}
import Safe {- safe -}

import qualified Music.Theory.List as T
import qualified Music.Theory.Set.List as T

import qualified Music.Theory.Z as Z
import qualified Music.Theory.Z.Drape_1999 as Z
import qualified Music.Theory.Z.Forte_1973 as Z
import qualified Music.Theory.Z.SRO as Z
import qualified Music.Theory.Z.TTO as Z

import Music.Theory.Z12 (Z12)
import qualified Music.Theory.Z12 as Z12
import qualified Music.Theory.Z12.Forte_1973 as Z12
import qualified Music.Theory.Z12.TTO as Z12
import qualified Music.Theory.Z12.SRO as Z12

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
issb :: [Z12] -> [Z12] -> [String]
issb p q =
    let k = length q - length p
        f = any id . map (\x -> Z12.forte_prime (p ++ x) == q) . Z12.tto_ti_related
    in map Z12.sc_name (filter f (Z.cf [k] Z12.scs))

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

{- | Pitch-class invariances (called @pi@ at @pct@).

>>> pct pi 0236 12
pcseg 0236
pcseg 6320
pcseg 532B
pcseg B235

> pci [1,2] [0,2,3,6] == [[0,2,3,6],[5,3,2,11],[6,3,2,0],[11,2,3,5]]

-}
pci :: [Int] -> [Z12] -> [[Z12]]
pci i p =
    let f q = T.set (map (q !!) i)
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
rsg x y = filter (\o -> Z.sro id o x == y) (Z.z_sro_univ (length x) id)

-- | Subsets.
sb :: [[Z12]] -> [[Z12]]
sb xs =
    let f p = all id (map (\q -> Z.has_sc Z.mod12 p q) xs)
    in filter f Z12.scs

{- | scc = set class completion

>>> pct scc 6-32 168
35A
49B
3AB
34B

> scc (Z12.sc "6-32") [1,6,8] == [[3,5,10],[4,9,11],[3,10,11],[3,4,11]]

-}
scc :: [Z12] -> [Z12] -> [[Z12]]
scc r p = map (\\ p) (filter (T.is_subset p) (Z12.tto_ti_related r))

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
        p_icv = Z12.to_Z12 n : Z12.icv p
        gen_si x = let x_f = Z12.forte_prime x
                       Just x_o = rs1 x_f x
                   in (nub (sort x),x_o,x_f)
    in (gen_si p,p_icv,Z.tics id p,gen_si (Z12.complement p),gen_si (map (* 5) p))

si_raw_pp :: [Z12] -> [String]
si_raw_pp p =
    let pf_pp concise (x_o,x_f) =
            concat [Z.tto_pp x_o," ",Z12.sc_name x_f
                   ,if concise then "" else Z12.z12_vec_pp x_f]
        si_pp (x,x_o,x_f) = concat [Z12.z12_set_pp x," (",pf_pp True (x_o,x_f),")"]
        ((p',p_o,p_f),p_icv,p_tics,c,m) = si_raw p
    in [Z12.z12_set_pp p'
       ,pf_pp False (p_o,p_f)
       ,Z12.z12_vec_pp p_icv
       ,Z.z16_vec_pp p_tics
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

> putStr $ unlines $ si [0,5,3,11]
-}
si :: [Z12] -> [String]
si p = zipWith (\k v -> concat [k,": ",v]) si_hdr (si_raw_pp p)
