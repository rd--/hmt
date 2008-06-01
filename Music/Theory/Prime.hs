module Music.Theory.Prime (prime) where

import Data.List
import Music.Theory.Pitch

prime :: (Integral a) => ([a] -> [a] -> Ordering) -> [a] -> [a]
prime _ [] = []
prime f p = minimumBy f (map (transposeTo 0) r)
    where q = invert 0 p
          r = rotations (pcset p) ++ rotations (pcset q)
