module Music.Theory.Prime (prime) where

import Music.Theory.Pitch (pcset, invert, rotations, transposeTo)
import Data.List (minimumBy)

-- | Forte comparison is odd, the rightmost interval is compared
-- | first, then the left intervals in order.

fcompare :: (Ord t) => [t] -> [t] -> Ordering
fcompare [] [] = EQ
fcompare p  q  = if r == EQ then compare p q else r
    where r = compare (last p) (last q)

prime :: (Integral a) => [a] -> [a]
prime p = minimumBy fcompare (map (transposeTo 0) r)
    where q = invert 0 p
          r = rotations (pcset p) ++ rotations (pcset q)
