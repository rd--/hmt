module Music.Theory.Pct where

import Data.Function
import Data.List
import Music.Theory.Prime
import Music.Theory.PitchClass
import Music.Theory.Set
import Music.Theory.Table

-- | Basic interval pattern, see Allen Forte \"The Basic Interval Patterns\"
-- /JMT/ 17/2 (1973):234-272
--
-- >>> bip 0t95728e3416
-- 11223344556
--
-- > bip [0,10,9,5,7,2,8,11,3,4,1,6] == [1,1,2,2,3,3,4,4,5,5,6]
-- > bip (pco "0t95728e3416") == [1,1,2,2,3,3,4,4,5,5,6]
bip :: (Integral a) => [a] -> [a]
bip = sort . map ic . int

-- | Cardinality filter
cf :: (Integral n) => [n] -> [[a]] -> [[a]]
cf ns = filter (\p -> genericLength p `elem` ns)

cgg :: [[a]] -> [[a]]
cgg [] = [[]]
cgg (x:xs) = [ y:z | y <- x, z <- cgg xs ]

-- | Combinations generator (cg == poweset)
cg :: [a] -> [[a]]
cg = powerset

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
ciseg :: (Integral a) => [a] -> [a]
ciseg = int . cyc

-- | pcset complement.
--
-- >>> cmpl 02468t
-- 13579B
--
-- > cmpl [0,2,4,6,8,10] == [1,3,5,7,9,11]
cmpl :: (Integral a) => [a] -> [a]
cmpl = ([0..11] \\) . pcset

-- | Form cycle.
--
-- >>> cyc 056
-- 0560
--
-- > cyc [0,5,6] == [0,5,6,0]
cyc :: [a] -> [a]
cyc [] = []
cyc (x:xs) = (x:xs) ++ [x]

-- | Diatonic implications.
dim :: (Integral a) => [a] -> [(a, [a])]
dim p =
    let g (i,q) = is_subset p (tn i q)
        f = filter g . zip [0..11] . repeat
        d = [0,2,4,5,7,9,11]
        m = [0,2,3,5,7,9,11]
        o = [0,1,3,4,6,7,9,10]
    in f d ++ f m ++ f o

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
-- > doi 2 (sc "7-35") [0,1,2,3,4] == [[1,3,5,6,8,10,11]]
doi :: (Integral a) => Int -> [a] -> [a] -> [[a]]
doi n p q =
    let f j = [pcset (tn j p), pcset (tni j p)]
        xs = concatMap f [0..11]
    in set (filter (\x -> length (x `intersect` q) == n) xs)

-- | Forte name.
fn :: (Integral a) => [a] -> String
fn = sc_name

-- | p `has_ess` q is true iff p can embed q in sequence.
has_ess :: (Integral a) => [a] -> [a] -> Bool
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
ess :: (Integral a) => [a] -> [a] -> [[a]]
ess p = filter (`has_ess` p) . all_RTnMI

-- | Can the set-class q (under prime form algorithm pf) be
--   drawn from the pcset p.
has_sc_pf :: (Integral a) => ([a] -> [a]) -> [a] -> [a] -> Bool
has_sc_pf pf p q =
    let n = length q
    in q `elem` map pf (cf [n] (powerset p))

-- | Can the set-class q be drawn from the pcset p.
has_sc :: (Integral a) => [a] -> [a] -> Bool
has_sc = has_sc_pf forte_prime

-- | Interval cycle filter.
--
-- >>> echo 22341 | icf
-- 22341
--
-- > icf [[2,2,3,4,1]] == [[2,2,3,4,1]]
icf :: (Num a) => [[a]] -> [[a]]
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
icseg :: (Integral a) => [a] -> [a]
icseg = map ic . iseg

-- | Interval segment (INT).
iseg :: (Integral a) => [a] -> [a]
iseg = int

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
-- > issb (sc "3-7") (sc "6-32") == ["3-2","3-7","3-11"]
issb :: (Integral a) => [a] -> [a] -> [String]
issb p q =
    let k = length q - length p
        f = any id . map (\x -> forte_prime (p ++ x) == q) . all_TnI
    in map sc_name (filter f (cf [k] scs))

-- | Matrix search.
--
-- >>> mxs 024579 642 | sort -u
-- 6421B9
-- B97642
--
-- > set (mxs [0,2,4,5,7,9] [6,4,2]) == [[6,4,2,1,11,9],[11,9,7,6,4,2]]
mxs :: (Integral a) => [a] -> [a] -> [[a]]
mxs p q = filter (q `isInfixOf`) (all_RTnI p)

-- | Normalize.
--
-- >>> nrm 0123456543210
-- 0123456
--
-- > nrm [0,1,2,3,4,5,6,5,4,3,2,1,0] == [0,1,2,3,4,5,6]
nrm :: (Ord a) => [a] -> [a]
nrm = set

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
pci :: (Integral a) => [a] -> [a] -> [[a]]
pci p i =
    let f q = set (map (q `genericIndex`) i)
    in filter (\q -> f q == f p) (all_RTnI p)

-- | Relate sets.
rs :: (Integral a) => [a] -> [a] -> [(SRO a, [a])]
rs x y =
    let xs = map (\o -> (o, o `sro` x)) sro_TnMI
        q = set y
    in filter (\(_,p) -> set p == q) xs

-- | Relate segments.
rsg :: (Integral a) => [a] -> [a] -> [(SRO a, [a])]
rsg x y = filter (\(_,x') -> x' == y) (sros x)

-- | Subsets.
sb :: (Integral a) => [[a]] -> [[a]]
sb xs =
    let f p = all id (map (`has_sc` p) xs)
    in filter f scs

-- | Super set-class.
--
-- >>> spsc 4-11 4-12
-- 5-26[02458]
--
-- > spsc [sc "4-11", sc "4-12"] == ["5-26"]
--
-- >>> spsc 3-11 3-8
-- 4-27[0258]
-- 4-Z29[0137]
--
-- > spsc [sc "3-11", sc "3-8"] == ["4-27","4-Z29"]
--
-- >>> spsc `fl 3`
-- 6-Z17[012478]
--
-- > spsc (cf [3] scs) == ["6-Z17"]
spsc :: (Integral a) => [[a]] -> [String]
spsc xs =
    let f y = all (y `has_sc`) xs
        g = (==) `on` length
    in (map sc_name . head . groupBy g . filter f) scs
