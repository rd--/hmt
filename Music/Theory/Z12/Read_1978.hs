-- | Ronald C. Read. \"Every one a winner or how to avoid isomorphism
-- search when cataloguing combinatorial configurations.\" /Annals of
-- Discrete Mathematics/ 2:107–20, 1978.
module Music.Theory.Z12.Read_1978 where

import Music.Theory.Z12 {- hmt -}
import qualified Music.Theory.Z.Read_1978 as Z {- hmt -}

type Code = Z.Code

-- | Encoder for 'encode_prime'.
--
-- > encode [0,1,3,6,8,9] == 843
encode :: [Z12] -> Code
encode = Z.encode

-- | Decoder for 'encode_prime'.
--
-- > decode 843 == [0,1,3,6,8,9]
decode :: Code -> [Z12]
decode = Z.decode 12

-- | Binary encoding prime form algorithm, equalivalent to Rahn.
--
-- > encode_prime [0,1,3,6,8,9] == [0,2,3,6,7,9]
-- > Music.Theory.Z12.Rahn_1980.rahn_prime [0,1,3,6,8,9] == [0,2,3,6,7,9]
encode_prime :: [Z12] -> [Z12]
encode_prime = Z.encode_prime id
