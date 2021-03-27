-- | Z-/n/ functions
module Music.Theory.Z where

import Data.Char {- base -}
import Data.List {- base -}

import qualified Music.Theory.List as T {- hmt -}

-- | Z type.
--
-- > map z_modulus [z7,z12] == [7,12]
newtype Z i = Z {z_modulus :: i}

-- | 'mod' of 'Z'.
--
-- > map (z_mod z12) [-1,0,1,11,12,13] == [11,0,1,11,0,1]
z_mod :: Integral i => Z i -> i -> i
z_mod (Z i) n = mod n i

-- | Common moduli in music theory.
z5,z7,z12,z16 :: Num i => Z i
z5 = Z 5
z7 = Z 7
z12 = Z 12
z16 = Z 16

-- | Is /n/ in (0,/m/-1).
is_z_n :: (Num a, Ord a) => a -> a -> Bool
is_z_n m n = n >= 0 && n < m

lift_unary_Z :: Integral i => Z i -> (t -> i) -> t -> i
lift_unary_Z z f = z_mod z . f

lift_binary_Z :: Integral i => Z i -> (s -> t -> i) -> s -> t -> i
lift_binary_Z z f n1 = z_mod z . f n1

-- | Add two Z.
--
-- > map (z_add z12 4) [1,5,6,11] == [5,9,10,3]
z_add :: Integral i => Z i -> i -> i -> i
z_add z = lift_binary_Z z (+)

-- | The underlying type /i/ is presumed to be signed...
--
-- > z_sub z12 0 8 == 4
--
-- > import Data.Word {- base -}
-- > z_sub z12 (0::Word8) 8 == 8
-- > ((0 - 8) :: Word8) == 248
-- > 248 `mod` 12 == 8
z_sub :: Integral i => Z i -> i -> i -> i
z_sub z = lift_binary_Z z (-)

-- | Allowing unsigned /i/ is rather inefficient...
--
-- > z_sub_unsigned z12 (0::Word8) 8 == 4
z_sub_unsigned :: (Integral i,Ord i) => Z i -> i -> i -> i
z_sub_unsigned z p q =
    if p > q
    then z_mod z (p - q)
    else z_mod z (p + z_modulus z - q)

z_mul :: Integral i => Z i -> i -> i -> i
z_mul z = lift_binary_Z z (*)

-- > z_negate z12 7 == 5
z_negate :: Integral i => Z i -> i -> i
z_negate z = z_sub z 0 -- error "Z numbers are not signed"

z_fromInteger :: Integral i => Z i -> Integer -> i
z_fromInteger z i = z_mod z (fromInteger i)

z_signum :: t -> u -> v
z_signum _ _ = error "Z numbers are not signed"

z_abs :: t -> u -> v
z_abs _ _ = error "Z numbers are not signed"

-- > map (to_Z z12) [-9,-3,0] == [3,9,0]
to_Z :: Integral i => Z i -> i -> i
to_Z z = z_fromInteger z . fromIntegral

from_Z :: (Integral i,Num n) => i -> n
from_Z = fromIntegral

-- | Universe of 'Z'.
--
-- > z_univ z12 == [0..11]
z_univ :: Integral i => Z i -> [i]
z_univ (Z z) = [0 .. z - 1]

-- | Z of 'z_univ' not in given set.
--
-- > z_complement z5 [0,2,3] == [1,4]
-- > z_complement z12 [0,2,4,5,7,9,11] == [1,3,6,8,10]
z_complement :: Integral i => Z i -> [i] -> [i]
z_complement z = (\\) (z_univ z)

z_quot :: Integral i => Z i -> i -> i -> i
z_quot z p = to_Z z . quot p

z_rem :: Integral i => Z i -> i -> i -> i
z_rem z p = to_Z z . rem p

div_err :: Integral i => String -> i -> i -> i
div_err s p q = if q == 0 then error ("div_err: zero" ++ s) else p `div` q

z_div :: Integral i => Z i -> i -> i -> i
z_div z p = to_Z z . div_err "z_div" p

z_quotRem :: Integral i => Z i -> i -> i -> (i,i)
z_quotRem z p q = (z_quot z p q,z_quot z p q)

z_divMod :: Integral i => Z i -> i -> i -> (i,i)
z_divMod z p q = (z_div z p q,z_mod z (mod p q))

z_toInteger :: Integral i => Z i -> i -> i
z_toInteger = to_Z

-- * Z16

-- | Type generalised 'intToDigit'.
--
-- > map integral_to_digit [0 .. 15] == "0123456789abcdef"
integral_to_digit :: Integral t => t -> Char
integral_to_digit = intToDigit . fromIntegral

-- | 'is_z_n' 16.
is_z16 :: Integral t => t -> Bool
is_z16 = is_z_n 16

-- | Alias for 'integral_to_digit'.
z16_to_char :: Integral t => t -> Char
z16_to_char = integral_to_digit

-- | 'z16_to_char' in braces, {1,2,3}.
z16_set_pp :: Integral t => [t] -> String
z16_set_pp = T.bracket ('{','}') . map z16_to_char

-- | 'z16_to_char' in arrows, <1,2,3>.
z16_seq_pp :: Integral t => [t] -> String
z16_seq_pp = T.bracket ('<','>') . map z16_to_char

-- | 'z16_to_char' in brackets, [1,2,3].
z16_vec_pp :: Integral t => [t] -> String
z16_vec_pp = T.bracket ('[',']') . map z16_to_char
