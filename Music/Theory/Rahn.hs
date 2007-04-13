module Music.Theory.Rahn (prime) where

import qualified Music.Theory.Prime as P

-- | Rahn comparison is rightmost inwards.
prime :: (Integral a) => [a] -> [a]
prime = P.prime cmp

cmp :: (Ord t) => [t] -> [t] -> Ordering
cmp p q = compare (reverse p) (reverse q)
