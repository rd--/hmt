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

all_Tn :: (Integral a) => [a] -> [[a]]
all_Tn p = map (`tn` p) [0..11]

all_TnI :: (Integral a) => [a] -> [[a]]
all_TnI p = let ps = all_Tn p in ps ++ map (invert 0) ps

all_RTnI :: (Integral a) => [a] -> [[a]]
all_RTnI p = let ps = all_TnI p in ps ++ map reverse ps

all_TnMI :: (Integral a) => [a] -> [[a]]
all_TnMI p = let ps = all_TnI p in ps ++ map m5 ps

all_RTnMI :: (Integral a) => [a] -> [[a]]
all_RTnMI p = let ps = all_TnMI p in ps ++ map reverse ps

all_rRTnMI :: (Integral a) => [a] -> [[a]]
all_rRTnMI p = map snd (sros p)

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

sro_Tn :: (Integral a) => [SRO a]
sro_Tn = [ SRO 0 False n False False | 
           n <- [0..11] ]

sro_TnI :: (Integral a) => [SRO a]
sro_TnI = [ SRO 0 False n False i | 
            n <- [0..11], 
            i <- [False, True] ]

sro_RTnI :: (Integral a) => [SRO a]
sro_RTnI = [ SRO 0 r n False i | 
             r <- [True, False],
             n <- [0..11], 
             i <- [False, True] ] 

sro_TnMI :: (Integral a) => [SRO a]
sro_TnMI = [ SRO 0 False n m i | 
             n <- [0..11], 
             m <- [True, False], 
             i <- [True, False] ]

sro_RTnMI :: (Integral a) => [SRO a]
sro_RTnMI = [ SRO 0 r n m i | 
              r <- [True, False],
              n <- [0..11],
              m <- [True, False],
              i <- [True, False] ]

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

-- | Is p a subset of q.
is_subset :: Eq a => [a] -> [a] -> Bool
is_subset p q = p `intersect` q == p

-- | Is p a superset of q.
is_superset :: Eq a => [a] -> [a] -> Bool
is_superset = flip is_subset
