module Music.Theory.Forte (forte_prime) where

import Music.Theory.Prime

cmp :: (Ord t) => [t] -> [t] -> Ordering
cmp [] [] = EQ
cmp p  q  = if r == EQ then compare p q else r
    where r = compare (last p) (last q)

-- | Forte comparison is rightmost first then leftmost outwards.
forte_prime :: (Integral a) => [a] -> [a]
forte_prime = prime cmp
