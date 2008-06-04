module Music.Theory.Pitch where

import Music.Theory.Set
import Data.Maybe
import Data.List

-- | Modulo twelve.
mod12 :: (Integral a) => a -> a
mod12 = (`mod` 12)

-- | Pitch class.
pc :: (Integral a) => a -> a
pc = mod12

-- | Remove duplicate elements and sort.
set :: (Ord a) => [a] -> [a]
set = sort . nub

-- | Map to pitch-class and reduce to set.
pcset :: (Integral a) => [a] -> [a]
pcset = set . map pc

-- | Transpose by n.
tn :: (Integral a) => a -> [a] -> [a]
tn n = map (pc . (+ n))

-- | Transpose so first element is n.
transposeTo :: (Integral a) => a -> [a] -> [a]
transposeTo _ [] = []
transposeTo n (x:xs) = n : tn (n - x) xs

-- | All transpositions.
transpositions :: (Integral a) => [a] -> [[a]]
transpositions p = map (`tn` p) [0..11]

-- | Invert about n.
invert :: (Integral a) => a -> [a] -> [a]
invert n = map (pc . (\p -> n - (p - n)))

-- | Invert about first element.
invertSelf :: (Integral a) => [a] -> [a]
invertSelf [] = []
invertSelf (x:xs) = invert x (x:xs)

-- | Composition on inversion about zero and transpose.
tni :: (Integral a) => a -> [a] -> [a]
tni n = tn n . invert 0

-- | Rotate left by n places.
rotate :: (Integral n) => n -> [a] -> [a]
rotate n p = a ++ b 
     where m = n `mod` genericLength p
           (b, a) = genericSplitAt m p

-- | Rotate right by n places.
rotate_right :: (Integral n) => n -> [a] -> [a]
rotate_right n = rotate (negate n)

-- | All rotations.
rotations :: [a] -> [[a]]
rotations p = map (`rotate` p) [0 .. length p - 1]

-- | Modulo 12 multiplication
mn :: (Integral a) => a -> [a] -> [a]
mn n = map (pc . (* n))

-- | M5
m5 :: (Integral a) => [a] -> [a]
m5 = mn 5

-- | Serial Operator, of the form rRTMI.
data SRO a = SRO a Bool a Bool Bool
             deriving (Eq, Show)

-- | Serial operation.
sro :: (Integral a) => SRO a -> [a] -> [a]
sro (SRO r r' t m i) x = let x1 = if i then invert 0 x else x
                             x2 = if m then m5 x1 else x1
                             x3 = tn t x2
                             x4 = if r' then reverse x3 else x3
                         in rotate r x4

-- | The total set of serial operations.
sros :: (Integral a) => [a] -> [(SRO a, [a])]
sros x = [ let o = (SRO r r' t m i) in (o, sro o x) | 
           r <- [0 .. genericLength x - 1], 
           r' <- [False, True], 
           t <- [0 .. 11], 
           m <- [False, True], 
           i <- [False, True] ]

-- | Relate segments.
rsg :: (Integral a) => [a] -> [a] -> [(SRO a, [a])]
rsg x y = filter (\(_,x') -> x' == y) (sros x)

-- | Intervals to values, zero is n.
dx_d :: (Num a) => a -> [a] -> [a]
dx_d n = scanl (+) n

-- | Integrate.
d_dx :: (Num a) => [a] -> [a]
d_dx [] = []
d_dx (_:[]) = []
d_dx (x:xs) = zipWith (-) xs (x:xs)

-- | Morris INT operator.
int :: (Integral a) => [a] -> [a]
int = map mod12 . d_dx

-- | Interval segment (INT).
iseg :: (Integral a) => [a] -> [a]
iseg = int

-- | Cyclic interval segment.
ciseg :: (Integral a) => [a] -> [a]
ciseg [] = []
ciseg (x:xs) = int ((x:xs) ++ [x])

-- | Interval class.
ic :: (Integral a) => a -> a
ic i = if i' <= 6 then i' else 12 - i'
    where i' = mod12 i

-- | Elements of p not in q
difference :: (Eq a) => [a] -> [a] -> [a]
difference p q = filter f p
     where f e = e `notElem` q

-- | Pitch classes not in set.
complement :: (Integral a) => [a] -> [a]
complement = difference [0..11]

-- | Is p a subsequence of q.
subsequence :: (Eq a) => [a] -> [a] -> Bool
subsequence = isInfixOf

-- | The standard t-matrix of p.
tmatrix :: (Integral a) => [a] -> [[a]]
tmatrix p = map (`tn` p) (transposeTo 0 (invertSelf p))

-- | Interval class vector.
icv :: (Integral a) => [a] -> [a]
icv s = map (fromMaybe 0) k
    where i = map (ic . uncurry (-)) (dyads s)
          j = map f (group (sort i))
          k = map (`lookup` j) [1..6]
          f l = (head l, genericLength l)

-- | Basic interval pattern.
bip :: (Integral a) => [a] -> [a]
bip = sort . map ic . int

-- | Cardinality filter
cf :: (Integral n) => [n] -> [[a]] -> [[a]]
cf ns = filter (\p -> genericLength p `elem` ns)

-- | Is p a subset of q.
is_subset :: Eq a => [a] -> [a] -> Bool
is_subset p q = p `intersect` q == p

-- | Is p a superset of q.
is_superset :: Eq a => [a] -> [a] -> Bool
is_superset = flip is_subset

-- | Can the set-class q (under prime form pf) be drawn from the pcset p.
has_sc :: (Integral a) => ([a] -> [a]) -> [a] -> [a] -> Bool
has_sc pf q p = let n = length q
                in q `elem` map pf (cf [n] (powerset p))

ici :: (Num t) => [Int] -> [[t]]
ici xs = let is j = [[0], [1,11], [2,10], [3,9], [4,8], [5,7], [6]] !! j
             ys = map is xs
         in cgg ys

ici_c :: [Int] -> [[Int]]
ici_c [] = []
ici_c (x:xs) = map (x:) (ici xs)

cgg :: [[a]] -> [[a]]
cgg [] = [[]]
cgg (x:xs) = [ y:z | y <- x, z <- cgg xs ]

-- | Combinations generator (cg == poweset)
cg :: [a] -> [[a]]
cg = powerset

-- | Powerset filtered by cardinality.
cg_r :: (Integral n) => n -> [a] -> [[a]]
cg_r n x = cf [n] (cg x)

