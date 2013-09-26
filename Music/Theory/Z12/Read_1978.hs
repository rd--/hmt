-- | Ronald C. Read. \"Every one a winner or how to avoid isomorphism
-- search when cataloguing combinatorial configurations.\" /Annals of
-- Discrete Mathematics/ 2:107–20, 1978.
module Music.Theory.Z12.Read_1978 where

import Data.Bits
import Music.Theory.Z12
import Music.Theory.Z12.SRO

-- | Encoder for 'encode_prime'.
--
-- > encode [0,1,3,6,8,9] == 843
encode :: [Z12] -> Integer
encode = sum . map ((2 ^) . (from_Z12 :: Z12 -> Integer))

-- | Decoder for 'encode_prime'.
--
-- > decode 843 == [0,1,3,6,8,9]
decode :: Integer -> [Z12]
decode n =
    let f i = (i, testBit n i)
    in map (to_Z12 . fst) (filter snd (map f [0..11]))

-- | Binary encoding prime form algorithm, equalivalent to Rahn.
--
-- > encode_prime [0,1,3,6,8,9] == rahn_prime [0,1,3,6,8,9]
encode_prime :: [Z12] -> [Z12]
encode_prime s =
    let t = map (`tn` s) [0..11]
        c = t ++ map (invert 0) t
    in decode (minimum (map encode c))
