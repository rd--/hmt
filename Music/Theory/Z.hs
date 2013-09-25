-- | Generalised Z-/n/ functions.
module Music.Theory.Z where

import Data.List {- base -}

lift_unary_Z :: Integral a => a -> (t -> a) -> t -> a
lift_unary_Z z f n = mod (f n) z

lift_binary_Z :: Integral a => a -> (s -> t -> a) -> s -> t -> a
lift_binary_Z z f n1 n2 = mod (n1 `f` n2) z

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
to_Z :: Integral i => i -> Integer -> i
to_Z z = z_fromInteger z

from_Z :: (Integral i,Num n) => i -> n
from_Z = fromIntegral

-- | Z not in set.
--
-- > z_complement 5 [0,2,3] == [1,4]
-- > z_complement 12 [0,2,4,5,7,9,11] == [1,3,6,8,10]
z_complement :: (Enum a, Eq a, Num a) => a -> [a] -> [a]
z_complement z = (\\) [0 .. z - 1]
