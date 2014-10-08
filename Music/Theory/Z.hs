-- | Generalised Z-/n/ functions.
module Music.Theory.Z where

{-

From GHC 7.6 onwards there is the modular-arithmetic package, which subsumes this work.

{-# Language DataKinds #-}

import Data.Modular {- modular-arithmetic -}
import GHC.TypeLits {- base -}

type Z n = Mod Integer n

-- > map negate [0::Z12 .. 11] == [0,11,10,9,8,7,6,5,4,3,2,1]
-- > map (+ 5) [0::Z12 .. 11] == [5,6,7,8,9,10,11,0,1,2,3,4]
type Z12 = Mod Integer 12

-- > map invert [0::Z12 .. 11] == [0,11,10,9,8,7,6,5,4,3,2,1]
invert :: KnownNat n => Z n -> Z n
invert = negate

-}

import Data.List {- base -}

lift_unary_Z :: Integral a => a -> (t -> a) -> t -> a
lift_unary_Z z f n = mod (f n) z

lift_binary_Z :: Integral a => a -> (s -> t -> a) -> s -> t -> a
lift_binary_Z z f n1 n2 = mod (n1 `f` n2) z

-- > import Music.Theory.Z
-- > import qualified Music.Theory.Z12 as Z12
-- > z_mod 12 (6::Z12.Z12) 12
-- > z_add 12 (1::Z12.Z12) 5
-- > (1::Z12.Z12) + 5
-- > map (z_add 12 4) [1,5,6] == [5,9,10]
z_add :: Integral a => a -> a -> a -> a
z_add z = lift_binary_Z z (+)

z_sub :: Integral a => a -> a -> a -> a
z_sub z = lift_binary_Z z (-)

z_mul :: Integral a => a -> a -> a -> a
z_mul z = lift_binary_Z z (*)

z_negate :: Integral a => a -> a -> a
z_negate z = lift_unary_Z z negate

z_fromInteger :: Integral a => a -> Integer -> a
z_fromInteger z i = fromInteger i `mod` z

z_signum :: t -> t1 -> t2
z_signum _ _ = error "Z numbers are not signed"

z_abs :: t -> t1 -> t2
z_abs _ _ = error "Z numbers are not signed"

-- > map (to_Z 12) [-9,-3,0] == [3,9,0]
to_Z :: Integral i => i -> i -> i
to_Z z = z_fromInteger z . fromIntegral

from_Z :: (Integral i,Num n) => i -> n
from_Z = fromIntegral

-- | Z not in set.
--
-- > z_complement 5 [0,2,3] == [1,4]
-- > z_complement 12 [0,2,4,5,7,9,11] == [1,3,6,8,10]
z_complement :: (Enum a, Eq a, Num a) => a -> [a] -> [a]
z_complement z = (\\) [0 .. z - 1]

z_quot :: Integral i => i -> i -> i -> i
z_quot z p = to_Z z . quot p

z_rem :: Integral c => c -> c -> c -> c
z_rem z p = to_Z z . rem p

z_div :: Integral c => c -> c -> c -> c
z_div z p = to_Z z . div p

-- > z_mod 12 6 12
z_mod :: Integral c => c -> c -> c -> c
z_mod z p = to_Z z . mod p

z_quotRem :: Integral t => t -> t -> t -> (t, t)
z_quotRem z p q = (z_quot z p q,z_quot z p q)

z_divMod :: Integral t => t -> t -> t -> (t, t)
z_divMod z p q = (z_div z p q,z_mod z p q)

z_toInteger :: Integral i => i -> i -> i
z_toInteger z = to_Z z
