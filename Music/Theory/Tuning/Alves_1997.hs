{- | Bill Alves. \"Pleng: Composing for a Justly Tuned Gender
Barung\". 1/1: Journal of the Just Intonation Network, 1:4-11, Spring
1997.  <http://www2.hmc.edu/~alves/pleng.html>
-}
module Music.Theory.Tuning.Alves_1997 where

import Music.Theory.Tuning.Type {- hmt -}

{- $setup
>>> import Music.Theory.Tuning
-}

{- | Alves Slendro

>>> map (round . ratio_to_cents) alves_slendro_r
[0,231,498,765,996]
-}
alves_slendro_r :: [Rational]
alves_slendro_r = [1, 8 / 7, 4 / 3, 14 / 9, 16 / 9]

{- | HMC /slendro/ tuning.

>>> tn_cents_i alves_slendro
[0,231,498,765,996]

> import Music.Theory.Tuning.Scala
> scl <- scl_load "alves_slendro"
> tn_cents_i (scale_to_tuning scl) == tn_cents_i alves_slendro
True
-}
alves_slendro :: Tuning
alves_slendro = Tuning (Left alves_slendro_r) Nothing

{- | Alves Pelog

>>> map (round . ratio_to_cents) alves_pelog_bem_r
[0,231,316,702,814]
-}
alves_pelog_bem_r :: [Rational]
alves_pelog_bem_r = [1, 8 / 7, 6 / 5, 3 / 2, 8 / 5]

{- | HMC /pelog bem/ tuning.

>>> tn_cents_i alves_pelog_bem
[0,231,316,702,814]

> scl <- scl_load "alves_pelog"
> tn_cents_i (scale_to_tuning scl) == [0,231,316,471,702,814,969]
True
-}
alves_pelog_bem :: Tuning
alves_pelog_bem = Tuning (Left alves_pelog_bem_r) Nothing

{- | Alves Pelog Barang

>>> map (round . ratio_to_cents) alves_pelog_barang_r
[0,386,471,857,969]
-}
alves_pelog_barang_r :: [Rational]
alves_pelog_barang_r = [1, 5 / 4, 21 / 16, 105 / 64, 7 / 4]

{- | HMC /pelog barang/ tuning.

>>> tn_cents_i alves_pelog_barang
[0,386,471,857,969]
-}
alves_pelog_barang :: Tuning
alves_pelog_barang = Tuning (Left alves_pelog_barang_r) Nothing

{- | Alves Pelog 23467

>>> map (round . ratio_to_cents) alves_pelog_23467_r
[0,386,471,702,969]
-}
alves_pelog_23467_r :: [Rational]
alves_pelog_23467_r = [1, 5 / 4, 21 / 16, 3 / 2, 7 / 4]

{- | HMC /pelog 2,3,4,6,7/ tuning.

>>> tn_cents_i alves_pelog_23467
[0,386,471,702,969]
-}
alves_pelog_23467 :: Tuning
alves_pelog_23467 = Tuning (Left alves_pelog_23467_r) Nothing
