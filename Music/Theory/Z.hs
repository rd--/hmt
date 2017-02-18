-- | Z-/n/ functions with modulo function as parameter.
module Music.Theory.Z where

import Data.Char {- base -}
import Data.List {- base -}

import qualified Music.Theory.List as T {- hmt -}

-- | The modulo function for Z.
type Z t = (t -> t)

-- | Is /n/ in (0,/m/-1).
is_z_n :: (Num a, Ord a) => a -> a -> Bool
is_z_n m n = n >= 0 && n < m

mod5 :: Integral i => Z i
mod5 n = n `mod` 5

mod7 :: Integral i => Z i
mod7 n = n `mod` 7

mod12 :: Integral i => Z i
mod12 n = n `mod` 12

lift_unary_Z :: Z i -> (t -> i) -> t -> i
lift_unary_Z z f n = z (f n)

lift_binary_Z :: Z i -> (s -> t -> i) -> s -> t -> i
lift_binary_Z z f n1 n2 = z (n1 `f` n2)

-- > import Music.Theory.Z
-- > import qualified Music.Theory.Z12 as Z12
-- > z_add id (11::Z12.Z12) 5 == 4
-- > (11::Z12.Z12) + 5 == 4
-- > map (z_add mod12 4) [1,5,6] == [5,9,10]
z_add :: Integral i => Z i -> i -> i -> i
z_add z = lift_binary_Z z (+)

z_sub :: Integral i => Z i -> i -> i -> i
z_sub z = lift_binary_Z z (-)

z_mul :: Integral i => Z i -> i -> i -> i
z_mul z = lift_binary_Z z (*)

z_negate :: Integral i => Z i -> i -> i
z_negate z = lift_unary_Z z negate

z_fromInteger :: Integral i => Z i -> Integer -> i
z_fromInteger z i = z (fromInteger i)

z_signum :: t -> u -> v
z_signum _ _ = error "Z numbers are not signed"

z_abs :: t -> u -> v
z_abs _ _ = error "Z numbers are not signed"

-- > map (to_Z mod12) [-9,-3,0] == [3,9,0]
to_Z :: Integral i => Z i -> i -> i
to_Z z = z_fromInteger z . fromIntegral

from_Z :: (Integral i,Num n) => i -> n
from_Z = fromIntegral

-- | Modulus of /z/.
--
-- > z_modulus mod12 == 12
z_modulus :: Integral i => Z i -> i
z_modulus z = maybe (error "z_modulus") (fromIntegral . (+ 1)) (findIndex ((== 0) . z) [1..])

-- | Universe of 'Z'.
--
-- > z_univ mod12 == [0..11]
z_univ :: Integral i => Z i -> [i]
z_univ z = 0 : takeWhile ((> 0) . z) [1..]

-- | Z of 'z_univ' not in given set.
--
-- > z_complement mod5 [0,2,3] == [1,4]
-- > z_complement mod12 [0,2,4,5,7,9,11] == [1,3,6,8,10]
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

-- > z_mod mod12 6 12 == 6
z_mod :: Integral i => Z i -> i -> i -> i
z_mod z p = to_Z z . mod p

z_quotRem :: Integral i => Z i -> i -> i -> (i,i)
z_quotRem z p q = (z_quot z p q,z_quot z p q)

z_divMod :: Integral i => Z i -> i -> i -> (i,i)
z_divMod z p q = (z_div z p q,z_mod z p q)

z_toInteger :: Integral i => Z i -> i -> i
z_toInteger z = to_Z z

-- * Z16

mod16 :: Integral i => Z i
mod16 n = n `mod` 16

integral_to_digit :: Integral t => t -> Char
integral_to_digit = intToDigit . fromIntegral

is_z16 :: Integral t => t -> Bool
is_z16 = is_z_n 16

z16_to_char :: Integral t => t -> Char
z16_to_char = integral_to_digit

z16_set_pp :: Integral t => [t] -> String
z16_set_pp = T.bracket ('{','}') . map z16_to_char

z16_seq_pp :: Integral t => [t] -> String
z16_seq_pp = T.bracket ('<','>') . map z16_to_char

z16_vec_pp :: Integral t => [t] -> String
z16_vec_pp = T.bracket ('[',']') . map z16_to_char
