module Music.Theory.Prime ( cmp_prime
                          , forte_prime
                          , rahn_prime
                          , encode_prime ) where

import Data.Bits
import Data.List
import Music.Theory.Pitch

-- | Prime form rule requiring comparator.
cmp_prime :: (Integral a) => ([a] -> [a] -> Ordering) -> [a] -> [a]
cmp_prime _ [] = []
cmp_prime f p = minimumBy f (map (transposeTo 0) r)
    where q = invert 0 p
          r = rotations (pcset p) ++ rotations (pcset q)

-- | Forte comparison (rightmost first then leftmost outwards).
forte_cmp :: (Ord t) => [t] -> [t] -> Ordering
forte_cmp [] [] = EQ
forte_cmp p  q  = if r == EQ then compare p q else r
    where r = compare (last p) (last q)

-- | Forte prime form.
forte_prime :: (Integral a) => [a] -> [a]
forte_prime = cmp_prime forte_cmp

-- | Rahn prime form (comparison is rightmost inwards).
rahn_prime :: (Integral a) => [a] -> [a]
rahn_prime = cmp_prime (\p q -> compare (reverse p) (reverse q))

-- | Binary encoding prime form algorithm, equalivalent to Rahn.
encode_prime :: (Integral a, Bits a) => [a] -> [a]
encode_prime s = decode (minimum (map encode c))
    where t = map (`tn` s) [0..11]
          c = t ++ (map (invert 0) t)

encode :: (Integral a) => [a] -> a
encode s = sum (map (2 ^) s)

decode :: (Bits a, Integral a) => a -> [a]
decode n = map (fromIntegral . fst) (filter snd (map f [0..11]))
    where f i = (i, testBit n i)
