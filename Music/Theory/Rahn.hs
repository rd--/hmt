module Music.Theory.Rahn (rahn_prime) where

import Music.Theory.Prime

-- | Rahn comparison is rightmost inwards.
rahn_prime :: (Integral a) => [a] -> [a]
rahn_prime = prime (\p q -> compare (reverse p) (reverse q))

