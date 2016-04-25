{-# Language DataKinds #-}
{- | Z12

Z12 are modulo 12 integers.

> map signum [-1,0::Z12,1] == [1,0,1]
> map abs [-1,0::Z12,1] == [11,0,1]

Aspects of the 'Enum' instance are cyclic.

> pred (0::Z12) == 11
> succ (11::Z12) == 0

'Bounded' works

> [minBound::Z12 .. maxBound] == [0::Z12 .. 11]

-}
module Music.Theory.Z12 where

import Data.List {- base -}
import Data.Modular {- modular-arithmetic -}
import GHC.TypeLits {- base -}

-- | 'Mod' 'Int'.
type Z n = Mod Int n

-- | 'Z' 12.
--
-- > map negate [0::Z12 .. 0xB] == [0,0xB,0xA,9,8,7,6,5,4,3,2,1]
-- > map (+ 5) [0::Z12 .. 11] == [5,6,7,8,9,0xA,0xB,0,1,2,3,4]
type Z12 = Mod Int 12

-- | Cyclic form of 'enumFromThenTo'.
--
-- > [9::Z12,11 .. 3] == []
-- > enumFromThenTo_cyc (9::Z12) 11 3 == [9,11,1,3]
enumFromThenTo_cyc :: KnownNat n => Z n -> Z n -> Z n -> [Z n]
enumFromThenTo_cyc n m o =
    let m' = m + (m - n)
    in case compare m' o of
         LT -> n : enumFromThenTo_cyc m m' o
         EQ -> [n,m,o]
         GT -> [n,m]

-- | Cyclic form of 'enumFromTo'.
--
-- > [9::Z12 .. 3] == []
-- > enumFromTo_cyc (9::Z12) 3 == [9,10,11,0,1,2,3]
enumFromTo_cyc :: KnownNat n => Z n -> Z n -> [Z n]
enumFromTo_cyc n m =
    let n' = succ n
    in if n' == m then [n,m] else n : enumFromTo_cyc n' m

{-
-}

-- | Convert integral to 'Z12'.
--
-- > map to_Z12 [-9,-3,0,13] == [3,9,0,1]
to_Z12 :: Integral i => i -> Z12
to_Z12 = toMod . fromIntegral

-- | Convert 'Z12' to integral.
from_Z12 :: Integral i => Z12 -> i
from_Z12 = fromIntegral . unMod

-- | Z12 not in set.
--
-- > complement [0,2,4,5,7,9,11] == [1,3,6,8,10]
complement :: [Z12] -> [Z12]
complement = (\\) [0 .. 11]

-- | Z12 to character (10 -> A, 11 -> B).
--
-- > map z12_char [0 .. 11] == "0123456789AB"
z12_char :: Z12 -> Char
z12_char n = if n < 0 || n > 11 then error "z12_char" else "0123456789AB" !! (unMod n)
