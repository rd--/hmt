-- | John Rahn. /Basic Atonal Theory/. Longman, New York, 1980.
module Music.Theory.Z.Rahn_1980 where

import qualified Music.Theory.Math.Z as Z {- hmt-base -}

import qualified Music.Theory.Z.Forte_1973 as Forte {- hmt -}

{- | Rahn prime form (comparison is rightmost inwards).

>>> rahn_cmp [0,1,3,6,8,9] [0,2,3,6,7,9]
GT
-}
rahn_cmp :: Ord a => [a] -> [a] -> Ordering
rahn_cmp p q = compare (reverse p) (reverse q)

{- | Rahn prime form, ie. 'Forte.ti_cmp_prime' of 'rahn_cmp'.

>>> z_rahn_prime Z.z12 [0,1,3,6,8,9]
[0,2,3,6,7,9]
-}
z_rahn_prime :: Integral i => Z.Z i -> [i] -> [i]
z_rahn_prime z = Forte.z_ti_cmp_prime z rahn_cmp

{- | The six sets where the Forte and Rahn prime forms differ.
Given here in Forte prime form.

>>> all (\p -> Forte.z_forte_prime Z.z12 p /= z_rahn_prime Z.z12 p) rahn_forte_diff
True
-}
rahn_forte_diff :: Num n => [[n]]
rahn_forte_diff =
  [ [0, 1, 3, 7, 8] -- 5-20 [0,1,5,6,8]
  , [0, 1, 3, 5, 8, 9] -- 6-31 [0,1,4,5,7,9]
  , [0, 1, 3, 6, 8, 9] -- 6-Z29 [0,2,3,6,7,9]
  , [0, 1, 2, 4, 7, 8, 9] -- 7-20 [0,1,2,5,6,7,9]
  , [0, 1, 2, 3, 5, 8, 9] -- 7-Z18 [0,1,4,5,6,7,9]
  , [0, 1, 2, 4, 5, 7, 9, 10] -- 8-26 [0,1,3,4,5,7,8,10]
  ]

{- | The Forte table with the set in Rahn prime form.

>>> let pr = Forte.z_forte_prime Z.z12
>>> let f (n, p) = let q = pr p in if p == q then Nothing else Just (n, p, q)
>>> mapM_ print (Data.Maybe.mapMaybe f sc_table)
("5-20",[0,1,5,6,8],[0,1,3,7,8])
("6-Z29",[0,2,3,6,7,9],[0,1,3,6,8,9])
("6-31",[0,1,4,5,7,9],[0,1,3,5,8,9])
("7-Z18",[0,1,4,5,6,7,9],[0,1,2,3,5,8,9])
("7-20",[0,1,2,5,6,7,9],[0,1,2,4,7,8,9])
("8-26",[0,1,3,4,5,7,8,10],[0,1,2,4,5,7,9,10])
-}
sc_table :: Integral n => Forte.Sc_Table n
sc_table = map (\(n, p) -> (n, z_rahn_prime Z.z12 p)) Forte.sc_table
