-- | Forte, 1973
module Music.Theory.Z12.Forte_1973 where

import Data.List
import Music.Theory.List
import Music.Theory.Z12
import Music.Theory.Z12.SRO

-- | Transposition related rotations of /p/.
--
-- > t_related [0,1,3] == [[0,1,3],[0,2,11],[0,9,10]]
t_related :: [Z12] -> [[Z12]]
t_related p =
    let r = rotations (sort p)
    in map (transposeTo 0) r

-- | Transposition and inversion related rotations of /p/.
--
-- > ti_related [0,1,3] == [[0,1,3],[0,2,11],[0,9,10]
-- >                       ,[0,9,11],[0,2,3],[0,1,10]]
ti_related :: [Z12] -> [[Z12]]
ti_related p =
    let q = invert 0 p
        r = rotations (sort p) ++ rotations (sort q)
    in map (transposeTo 0) r

-- | Variant with default value for empty input list case.
minimumBy_or :: a -> (a -> a -> Ordering) -> [a] -> a
minimumBy_or p f q = if null q then p else minimumBy f q

-- | Prime form rule requiring comparator, considering 't_related'.
cmp_prime_t :: ([Z12] -> [Z12] -> Ordering) -> [Z12] -> [Z12]
cmp_prime_t f = minimumBy_or [] f . t_related

-- | Prime form rule requiring comparator, considering 'ti_related'.
cmp_prime_ti :: ([Z12] -> [Z12] -> Ordering) -> [Z12] -> [Z12]
cmp_prime_ti f = minimumBy_or [] f . ti_related

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
forte_prime = cmp_prime_ti forte_cmp
