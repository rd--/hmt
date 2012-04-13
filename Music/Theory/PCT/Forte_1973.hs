-- | Forte, 1973
module Music.Theory.PCT.Forte_1973 where

import Data.List
import Music.Theory.List
import Music.Theory.PCT.SRO
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
forte_prime :: [Z12] -> [Z12]
forte_prime = cmp_prime forte_cmp
