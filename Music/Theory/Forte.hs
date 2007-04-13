module Music.Theory.Forte (prime) where

import qualified Music.Theory.Prime as P

cmp :: (Ord t) => [t] -> [t] -> Ordering
cmp [] [] = EQ
cmp p  q  = if r == EQ then compare p q else r
    where r = compare (last p) (last q)

-- | Forte comparison is rightmost first then leftmost outwards.
prime :: (Integral a) => [a] -> [a]
prime = P.prime cmp
