module Music.Theory.Pitch where

import Music.Theory.Set
import Data.Maybe
import Data.List

-- | Modulo twelve.
mod12 :: (Integral a) => a -> a
mod12 = (flip mod) 12

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
transpositions p = map ((flip tn) p) [0..11]

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
rotations p = map f [0..length p - 1]
     where f i = rotate i p

-- | Intervals to values, zero is n.
dx_d :: (Num a) => a -> [a] -> [a]
dx_d n = scanl (+) n

-- | Morris INT operator.
d_dx :: (Num a) => [a] -> [a]
d_dx []     = []
d_dx [_]    = []
d_dx (x:xs) = zipWith (-) xs (x:xs)

-- | Morris INT operator.
int :: (Num a) => [a] -> [a]
int = d_dx

-- | Interval segment.
iseg :: (Integral a) => [a] -> [a]
iseg = (map mod12) . d_dx

-- | Cyclic interval segment.
ciseg :: (Integral a) => [a] -> [a]
ciseg [] = []
ciseg (x:xs) = iseg ((x:xs) ++ [x])

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
subsequence p q = not (isNothing r)
    where r = find (isPrefixOf p) (tails q)

-- | The standard t-matrix of p.
tmatrix :: (Integral a) => [a] -> [[a]]
tmatrix p = map ((flip tn) p) (transposeTo 0 (invertSelf p))

-- | Interval class vector.
icv :: (Integral a) => [a] -> [a]
icv s = map (fromMaybe 0) k
    where i = map (ic . uncurry (-)) (dyads s)
          j = map f (group (sort i))
          k = map ((flip lookup) j) [1..6]
          f l = (head l, genericLength l)

-- | Basic interval pattern.
bip :: (Integral a) => [a] -> [a]
bip = sort . map ic . iseg
