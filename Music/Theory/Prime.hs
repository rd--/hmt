-- | Forte and Rahn prime form operations.
module Music.Theory.Prime where

import Data.Bits
import Data.List
import Music.Theory.List
import Music.Theory.SRO
import Music.Theory.Z12

-- | Prime form rule requiring comparator.
cmp_prime :: ([Z12] -> [Z12] -> Ordering) -> [Z12] -> [Z12]
cmp_prime _ [] = []
cmp_prime f p =
    let q = invert 0 p
        r = rotations (sort p) ++ rotations (sort q)
    in minimumBy f (map (transposeTo 0) r)

-- | Forte comparison function (rightmost first then leftmost outwards).
--
-- > forte_cmp [0,1,3,6,8,9] [0,2,3,6,7,9] == LT
forte_cmp :: (Ord t) => [t] -> [t] -> Ordering
forte_cmp [] [] = EQ
forte_cmp p  q  =
    let r = compare (last p) (last q)
    in if r == EQ then compare p q else r

-- | Forte prime form, ie. 'cmp_prime' of 'forte_cmp'.
--
-- > forte_prime [0,1,3,6,8,9] == [0,1,3,6,8,9]
-- > forte_prime [0,1,3,6,8,9] /= rahn_prime [0,1,3,6,8,9]
forte_prime :: [Z12] -> [Z12]
forte_prime = cmp_prime forte_cmp

-- | Rahn prime form (comparison is rightmost inwards).
--
-- > rahn_cmp [0,1,3,6,8,9] [0,2,3,6,7,9] == GT
rahn_cmp :: Ord a => [a] -> [a] -> Ordering
rahn_cmp p q = compare (reverse p) (reverse q)

-- | Rahn prime form, ie. 'cmp_prime' of 'rahn_cmp'.
--
-- > rahn_prime [0,1,3,6,8,9] == [0,2,3,6,7,9]
rahn_prime :: [Z12] -> [Z12]
rahn_prime = cmp_prime rahn_cmp

-- | Encoder for 'encode_prime'.
--
-- > encode [0,1,3,6,8,9] == 843
encode :: [Z12] -> Integer
encode = sum . map (2 ^) . map (fromZ12::Z12->Integer)

-- | Decoder for 'encode_prime'.
--
-- > decode 843 == [0,1,3,6,8,9]
decode :: Integer -> [Z12]
decode n =
    let f i = (i, testBit n i)
    in map (toZ12 . fst) (filter snd (map f [0..11]))

-- | Binary encoding prime form algorithm, equalivalent to Rahn.
--
-- > encode_prime [0,1,3,6,8,9] == rahn_prime [0,1,3,6,8,9]
encode_prime :: [Z12] -> [Z12]
encode_prime s =
    let t = map (`tn` s) [0..11]
        c = t ++ map (invert 0) t
    in decode (minimum (map encode c))
