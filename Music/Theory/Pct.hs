module Music.Theory.Pct where

import Music.Theory.Prime
import Music.Theory.Pitch
import Music.Theory.Set
import Music.Theory.Table
import Data.List

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
cg_r n x = cf [n] (cg x)

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
dim p = let f = filter (\(i,q) -> is_subset p (tn i q)) . zip [0..11] . repeat
            d = [0,2,4,5,7,9,11]
            m = [0,2,3,5,7,9,11]
            o = [0,1,3,4,6,7,9,10]
        in f d ++ f m ++ f o

-- | Diatonic interval set to interval set.
dis :: (Integral t) => [Int] -> [t]
dis = concatMap (\j -> [[], [], [1,2], [3,4], [5,6], [6,7], [8,9], [10,11]] !! j)

-- | Degree of intersection.
doi :: (Integral a) => Int -> [a] -> [a] -> [[a]]
doi n p q = let xs = concatMap (\j -> [pcset (tn j p), pcset (tni j p)]) [0..11]
            in set (filter (\x -> length (x `intersect` q) == n) xs)

-- | Forte name.
fn :: (Integral a) => [a] -> String
fn = sc_name

-- | p `has_ess` q is true iff p can embed q in sequence.
has_ess :: (Integral a) => [a] -> [a] -> Bool
has_ess _ [] = True
has_ess [] _ = False
has_ess (p:ps) (q:qs) = if p == q then has_ess ps qs else has_ess ps (q:qs)

-- | Embedded segment search.
ess :: (Integral a) => [a] -> [a] -> [[a]]
ess p q = filter (`has_ess` p) (all_RTnMI q)

-- | Can the set-class q (under prime form pf) be drawn from the pcset p.
has_sc_pf :: (Integral a) => ([a] -> [a]) -> [a] -> [a] -> Bool
has_sc_pf pf p q = let n = length q
                in q `elem` map pf (cf [n] (powerset p))

-- | Can the set-class q (under prime form pf) be drawn from the pcset p.
has_sc :: (Integral a) => [a] -> [a] -> Bool
has_sc = has_sc_pf forte_prime

-- | Interval class set to interval sets.
ici :: (Num t) => [Int] -> [[t]]
ici xs = let is j = [[0], [1,11], [2,10], [3,9], [4,8], [5,7], [6]] !! j
             ys = map is xs
         in cgg ys

-- | Interval class set to interval sets, concise variant.
ici_c :: [Int] -> [[Int]]
ici_c [] = []
ici_c (x:xs) = map (x:) (ici xs)

-- | Interval segment (INT).
iseg :: (Integral a) => [a] -> [a]
iseg = int

-- | Relate segments.
rsg :: (Integral a) => [a] -> [a] -> [(SRO a, [a])]
rsg x y = filter (\(_,x') -> x' == y) (sros x)

-- | Super set-class.
spsc :: (Integral a) => [[a]] -> [String]
spsc xs = let f y = all (y `has_sc`) xs
              g p q = length p == length q
          in (map sc_name . head . groupBy g . filter f) scs
