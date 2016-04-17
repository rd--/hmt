{-# Language DataKinds #-}

-- | Generalised Z-/n/ functions.
module Music.Theory.Z.DK where

import Data.Modular {- modular-arithmetic -}
import GHC.TypeLits {- base -}

type Z n = Mod Integer n

-- > map negate [0::Z12 .. 0xB] == [0,0xB,0xA,9,8,7,6,5,4,3,2,1]
-- > map (+ 5) [0::Z12 .. 11] == [5,6,7,8,9,0xA,0xB,0,1,2,3,4]
type Z12 = Mod Integer 12

-- > map invert [0::Z12 .. 11] == [0,11,10,9,8,7,6,5,4,3,2,1]
invert :: KnownNat n => Z n -> Z n
invert = negate
