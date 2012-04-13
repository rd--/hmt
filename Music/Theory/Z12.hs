{-# Language GeneralizedNewtypeDeriving #-}
module Music.Theory.Z12 where

import Data.Bits

newtype Z12 = Z12 Int deriving (Eq,Ord,Enum,Bits,Bounded,Integral,Real)
instance Show Z12 where showsPrec p (Z12 i) = showsPrec p i

liftUZ12 :: (Int -> Int) -> Z12 -> Z12
liftUZ12 f (Z12 a) = Z12 (mod (f a) 12)

liftBZ12 :: (Int -> Int -> Int) -> Z12 -> Z12 -> Z12
liftBZ12 f (Z12 a) (Z12 b) = Z12 (mod (a `f` b) 12)

instance Num Z12 where
  (+) = liftBZ12 (+)
  (-) = liftBZ12 (-)
  (*) = liftBZ12 (*)
  negate = liftUZ12 negate
  fromInteger i = Z12 (fromInteger i `mod` 12)
  signum _ = error "Z12 numbers are not signed"
  abs _ = error "Z12 numbers are not signed"
