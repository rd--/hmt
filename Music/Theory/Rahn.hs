module Music.Theory.Rahn (prime) where

import qualified Music.Theory.Prime as P

-- | Rahn comparison is rightmost inwards.
prime :: (Integral a) => [a] -> [a]
prime = P.prime cmp

cmp :: (Ord t) => [t] -> [t] -> Ordering
cmp [] [] = EQ
cmp p  q  = if r == EQ then cmp p q else r
    where r = compare (last p) (last q)
