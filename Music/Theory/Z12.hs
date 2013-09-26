{-# Language GeneralizedNewtypeDeriving #-}
module Music.Theory.Z12 where

import Data.List {- base -}

-- | Z12 are modulo 12 integers.
--
-- > map signum [-1,0::Z12,1] == [1,0,1]
-- > map abs [-1,0::Z12,1] == [11,0,1]
newtype Z12 = Z12 Int deriving (Eq,Ord,Enum,Bounded,Integral,Real)

-- | The Z12 modulo (ie. @12@) as a 'Z12' value.  This is required
-- when lifting generalised @Z@ functions to 'Z12'.  It is /not/ the
-- same as writing @12::Z12@.
--
-- > z12_modulo == Z12 12
-- > z12_modulo /= 12
-- > (12::Z12) == 0
-- > show z12_modulo == "(Z12 12)"
z12_modulo :: Z12
z12_modulo = Z12 12

-- | Basis for Z12 show instance.
--
-- > map show [-1,0::Z12,1,z12_modulo] == ["11","0","1","(Z12 12)"]
z12_showsPrec :: Int -> Z12 -> ShowS
z12_showsPrec p (Z12 i) =
    let x = showsPrec p i
    in if i < 0 || i > 11
       then showString "(Z12 " . x . showString ")"
       else x

instance Show Z12 where showsPrec = z12_showsPrec

-- | Lift unary function over integers to Z12.
--
-- > lift_unary_Z12 (negate) 7 == 5
lift_unary_Z12 :: (Int -> Int) -> Z12 -> Z12
lift_unary_Z12 f (Z12 a) = Z12 (f a `mod` 12)

-- | Lift unary function over integers to Z12.
--
-- > map (lift_binary_Z12 (+) 4) [1,5,6] == [5,9,10]
lift_binary_Z12 :: (Int -> Int -> Int) -> Z12 -> Z12 -> Z12
lift_binary_Z12 f (Z12 a) (Z12 b) = Z12 (mod (a `f` b) 12)

check_negative :: (Int -> Int) -> Z12 -> Z12
check_negative f (Z12 n) =
    if n < 0
    then error "check_negative: negative Z12"
    else Z12 (f n)

instance Num Z12 where
  (+) = lift_binary_Z12 (+)
  (-) = lift_binary_Z12 (-)
  (*) = lift_binary_Z12 (*)
  negate = lift_unary_Z12 negate
  fromInteger n = Z12 (fromInteger n `mod` 12)
  signum = check_negative signum
  abs = check_negative abs

-- | Convert integral to 'Z12'.
--
-- > map to_Z12 [-9,-3,0,13] == [3,9,0,1]
to_Z12 :: Integral i => i -> Z12
to_Z12 = fromIntegral

-- | Convert 'Z12' to integral.
from_Z12 :: Integral i => Z12 -> i
from_Z12 = fromIntegral

-- | Z12 not in set.
--
-- > complement [0,2,4,5,7,9,11] == [1,3,6,8,10]
complement :: [Z12] -> [Z12]
complement = (\\) [0 .. 11]
