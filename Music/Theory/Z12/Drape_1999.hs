-- | Haskell implementations of @pct@ operations.
-- See <http://slavepianos.org/rd/?t=pct>.
module Music.Theory.Z12.Drape_1999 where

import Data.Function {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as T
import qualified Music.Theory.Set.List as T
import Music.Theory.Z12
import qualified Music.Theory.Z12.Forte_1973 as T
import qualified Music.Theory.Z12.Morris_1987 as T
import qualified Music.Theory.Z12.TTO as TTO
import qualified Music.Theory.Z12.SRO as SRO

-- | Cardinality filter
--
-- > cf [0,3] (cg [1..4]) == [[1,2,3],[1,2,4],[1,3,4],[2,3,4],[]]
cf :: (Integral n) => [n] -> [[a]] -> [[a]]
cf ns = filter (\p -> genericLength p `elem` ns)

-- | Combinatorial sets formed by considering each set as possible
-- values for slot.
--
-- > cgg [[0,1],[5,7],[3]] == [[0,5,3],[0,7,3],[1,5,3],[1,7,3]]
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
-- >>> cg -r3 0159
-- 015
-- 019
-- 059
-- 159
--
-- > cg_r 3 [0,1,5,9] == [[0,1,5],[0,1,9],[0,5,9],[1,5,9]]
cg_r :: (Integral n) => n -> [a] -> [[a]]
cg_r n = cf [n] . cg

-- | Cyclic interval segment.
ciseg :: [Z12] -> [Z12]
ciseg = T.int . cyc

-- | Synonynm for 'complement'.
--
-- >>> cmpl 02468t
-- 13579B
--
-- > cmpl [0,2,4,6,8,10] == [1,3,5,7,9,11]
cmpl :: [Z12] -> [Z12]
cmpl = complement

-- | Form cycle.
--
-- >>> cyc 056
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
    let g (i,q) = T.is_subset p (TTO.tn i q)
        f = filter g . zip [0..11] . repeat
        d = [0,2,4,5,7,9,11]
        m = [0,2,3,5,7,9,11]
        o = [0,1,3,4,6,7,9,10]
    in f d ++ f m ++ f o

-- | Variant of 'dim' that is closer to the 'pct' form.
--
-- >>> dim 016
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
-- >>> dis 24
-- 1256
--
-- > dis [2,4] == [1,2,5,6]
dis :: (Integral t) => [Int] -> [t]
dis =
    let is = [[], [], [1,2], [3,4], [5,6], [6,7], [8,9], [10,11]]
    in concatMap (\j -> is !! j)

-- | Degree of intersection.
--
-- >>> echo 024579e | doi 6 | sort -u
-- 024579A
-- 024679B
--
-- > let p = [0,2,4,5,7,9,11]
-- > in doi 6 p p == [[0,2,4,5,7,9,10],[0,2,4,6,7,9,11]]
--
-- >>> echo 01234 | doi 2 7-35 | sort -u
-- 13568AB
--
-- > doi 2 (T.sc "7-35") [0,1,2,3,4] == [[1,3,5,6,8,10,11]]
doi :: Int -> [Z12] -> [Z12] -> [[Z12]]
doi n p q =
    let f j = [TTO.tn j p,TTO.tni j p]
        xs = concatMap f [0..11]
    in T.set (filter (\x -> length (x `intersect` q) == n) xs)

-- | Forte name.
fn :: [Z12] -> String
fn = T.sc_name

-- | p `has_ess` q is true iff p can embed q in sequence.
has_ess :: [Z12] -> [Z12] -> Bool
has_ess _ [] = True
has_ess [] _ = False
has_ess (p:ps) (q:qs) = if p == q
                        then has_ess ps qs
                        else has_ess ps (q:qs)

-- | Embedded segment search.
--
-- >>> echo 23a | ess 0164325
-- 2B013A9
-- 923507A
--
-- > ess [2,3,10] [0,1,6,4,3,2,5] == [[9,2,3,5,0,7,10],[2,11,0,1,3,10,9]]
ess :: [Z12] -> [Z12] -> [[Z12]]
ess p = filter (`has_ess` p) . SRO.rtmi_related

-- | Can the set-class q (under prime form algorithm pf) be
--   drawn from the pcset p.
has_sc_pf :: (Integral a) => ([a] -> [a]) -> [a] -> [a] -> Bool
has_sc_pf pf p q =
    let n = length q
    in q `elem` map pf (cf [n] (cg p))

-- | Can the set-class q be drawn from the pcset p.
has_sc :: [Z12] -> [Z12] -> Bool
has_sc = has_sc_pf T.forte_prime

-- | Interval cycle filter.
--
-- >>> echo 22341 | icf
-- 22341
--
-- > icf [[2,2,3,4,1]] == [[2,2,3,4,1]]
icf :: (Num a,Eq a) => [[a]] -> [[a]]
icf = filter ((== 12) . sum)

-- | Interval class set to interval sets.
--
-- >>> ici -c 123
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
-- >>> icseg 013265e497t8
-- 12141655232
--
-- > icseg [0,1,3,2,6,5,11,4,9,7,10,8] == [1,2,1,4,1,6,5,5,2,3,2]
icseg :: [Z12] -> [Z12]
icseg = map T.ic . iseg

-- | Interval segment (INT).
iseg :: [Z12] -> [Z12]
iseg = T.int

-- | Imbrications.
imb :: (Integral n) => [n] -> [a] -> [[a]]
imb cs p =
    let g n = (== n) . genericLength
        f ps n = filter (g n) (map (genericTake n) ps)
    in concatMap (f (tails p)) cs

-- | 'issb' gives the set-classes that can append to 'p' to give 'q'.
--
-- >>> issb 3-7 6-32
-- 3-7
-- 3-2
-- 3-11
--
-- > issb (T.sc "3-7") (T.sc "6-32") == ["3-2","3-7","3-11"]
issb :: [Z12] -> [Z12] -> [String]
issb p q =
    let k = length q - length p
        f = any id . map (\x -> T.forte_prime (p ++ x) == q) . TTO.ti_related
    in map T.sc_name (filter f (cf [k] T.scs))

-- | Matrix search.
--
-- >>> mxs 024579 642 | sort -u
-- 6421B9
-- B97642
--
-- > T.set (mxs [0,2,4,5,7,9] [6,4,2]) == [[6,4,2,1,11,9],[11,9,7,6,4,2]]
mxs :: [Z12] -> [Z12] -> [[Z12]]
mxs p q = filter (q `isInfixOf`) (SRO.rti_related p)

-- | Normalize.
--
-- >>> nrm 0123456543210
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
-- >>> pi 0236 12
-- 0236
-- 6320
-- 532B
-- B235
--
-- > pci [0,2,3,6] [1,2] == [[0,2,3,6],[5,3,2,11],[6,3,2,0],[11,2,3,5]]
pci :: [Z12] -> [Z12] -> [[Z12]]
pci p i =
    let f q = T.set (map (q `genericIndex`) i)
    in filter (\q -> f q == f p) (SRO.rti_related p)

-- | Relate sets.
--
-- >>> rs 0123 641e
-- T1M
--
-- > import Music.Theory.Z12.Morris_1987.Parse
-- > rs [0,1,2,3] [6,4,1,11] == [(rnrtnmi "T1M",[1,6,11,4])
-- >                            ,(rnrtnmi "T4MI",[4,11,6,1])]
rs :: [Z12] -> [Z12] -> [(T.SRO, [Z12])]
rs x y =
    let xs = map (\o -> (o, o `T.sro` x)) T.sro_TnMI
        q = T.set y
    in filter (\(_,p) -> T.set p == q) xs

-- | Relate segments.
--
-- >>> rsg 156 3BA
-- T4I
--
-- > rsg [1,5,6] [3,11,10] == [rnrtnmi "T4I",rnrtnmi "r1RT4MI"]
--
-- >>> rsg 0123 05t3
-- T0M
--
-- > rsg [0,1,2,3] [0,5,10,3] == [rnrtnmi "T0M",rnrtnmi "RT3MI"]
--
-- >>> rsg 0123 4e61
-- RT1M
--
-- > rsg [0,1,2,3] [4,11,6,1] == [rnrtnmi "T4MI",rnrtnmi "RT1M"]
--
-- >>> echo e614 | rsg 0123
-- r3RT1M
--
-- > rsg [0,1,2,3] [11,6,1,4] == [rnrtnmi "r1T4MI",rnrtnmi "r1RT1M"]
--
rsg :: [Z12] -> [Z12] -> [T.SRO]
rsg x y = map fst (filter (\(_,x') -> x' == y) (T.sros x))

-- | Subsets.
sb :: [[Z12]] -> [[Z12]]
sb xs =
    let f p = all id (map (`has_sc` p) xs)
    in filter f T.scs

-- | Super set-class.
--
-- >>> spsc 4-11 4-12
-- 5-26[02458]
--
-- > spsc [T.sc "4-11",T.sc "4-12"] == ["5-26"]
--
-- >>> spsc 3-11 3-8
-- 4-27[0258]
-- 4-Z29[0137]
--
-- > spsc [T.sc "3-11",T.sc "3-8"] == ["4-27","4-Z29"]
--
-- >>> spsc `fl 3`
-- 6-Z17[012478]
--
-- > spsc (cf [3] scs) == ["6-Z17"]
spsc :: [[Z12]] -> [String]
spsc xs =
    let f y = all (y `has_sc`) xs
        g = (==) `on` length
    in (map T.sc_name . head . groupBy g . filter f) T.scs
