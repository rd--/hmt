module Music.Theory.Encode (prime) where

import Data.Bits
import Music.Theory.Pitch

-- | Binary encoding prime form algorithm, equalivalent to Rahn.
prime :: (Integral a, Bits a) => [a] -> [a]
prime s = decode (minimum (map encode c))
    where t = map ((flip tn) s) [0..11]
          c = t ++ (map (invert 0) t)

encode :: (Integral a) => [a] -> a
encode s = sum (map (2 ^) s)

decode :: (Bits a, Integral a) => a -> [a]
decode n = map (fromIntegral . fst) (filter snd (map f [0..11]))
    where f i = (i, testBit n i)
