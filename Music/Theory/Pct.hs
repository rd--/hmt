module Music.Theory.Pct where

import Data.Function
import Data.List
import Music.Theory.Prime
import Music.Theory.Pitch
import Music.Theory.Set
import Music.Theory.Table

-- | Basic interval pattern.
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
cg_r :: (Integral n) => n -> [a] -> [[a]]
cg_r n = cf [n] . cg

-- | Cyclic interval segment.
ciseg :: (Integral a) => [a] -> [a]
ciseg = int . cyc

-- | pcset complement.
cmpl :: (Integral a) => [a] -> [a]
cmpl = ([0..11] \\) . pcset

-- | Form cycle.
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
dis :: (Integral t) => [Int] -> [t]
dis =
    let is = [[], [], [1,2], [3,4], [5,6], [6,7], [8,9], [10,11]]
    in concatMap (\j -> is !! j)

-- | Degree of intersection.
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
icf :: (Num a) => [[a]] -> [[a]]
icf = filter ((== 12) . sum)

-- | Interval class set to interval sets.
ici :: (Num t) => [Int] -> [[t]]
ici xs =
    let is j = [[0], [1,11], [2,10], [3,9], [4,8], [5,7], [6]] !! j
        ys = map is xs
    in cgg ys

-- | Interval class set to interval sets, concise variant.
ici_c :: [Int] -> [[Int]]
ici_c [] = []
ici_c (x:xs) = map (x:) (ici xs)

-- | Interval-class segment.
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

-- | p `issb` q gives the set-classes that can append to p to give q.
issb :: (Integral a) => [a] -> [a] -> [String]
issb p q =
    let k = length q - length p
        f = any id . map (\x -> forte_prime (p ++ x) == q) . all_TnI
    in map sc_name (filter f (cf [k] scs))

-- | Matrix search.
mxs :: (Integral a) => [a] -> [a] -> [[a]]
mxs p q = filter (q `isInfixOf`) (all_RTnI p)

-- | Normalize.
nrm :: (Ord a) => [a] -> [a]
nrm = set

-- | Normalize, retain duplicate elements.
nrm_r :: (Ord a) => [a] -> [a]
nrm_r = sort

-- | Pitch-class invariances.
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
spsc :: (Integral a) => [[a]] -> [String]
spsc xs =
    let f y = all (y `has_sc`) xs
        g = (==) `on` length
    in (map sc_name . head . groupBy g . filter f) scs
