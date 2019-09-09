-- | Bill Alves. \"Pleng: Composing for a Justly Tuned Gender
-- Barung\". 1/1: Journal of the Just Intonation Network, 1:4-11, Spring
-- 1997.  <http://www2.hmc.edu/~alves/pleng.html>
module Music.Theory.Tuning.Alves_1997 where

import Music.Theory.Tuning.Type

-- > let c = [0,231,498,765,996]
-- > in map (round.to_cents_r) alves_slendro_r == c
alves_slendro_r :: [Rational]
alves_slendro_r = [1,8/7,4/3,14/9,16/9]

-- | HMC /slendro/ tuning.
--
-- > cents_i alves_slendro == [0,231,498,765,996]
--
-- > scl <- scl_load "slendro_alves"
-- > cents_i (scale_tuning 0.01 scl) == cents_i alves_slendro
alves_slendro :: Tuning
alves_slendro = Tuning (Left alves_slendro_r) 2

-- > let c = [0,231,316,702,814]
-- > in map (round.to_cents_r) alves_pelog_bem_r == c
alves_pelog_bem_r :: [Rational]
alves_pelog_bem_r = [1,8/7,6/5,3/2,8/5]

-- | HMC /pelog bem/ tuning.
--
-- > cents_i alves_pelog_bem == [0,231,316,702,814]
--
-- > scl <- scl_load "pelog_alves"
-- > cents_i (scale_tuning 0.01 scl) == [0,231,316,471,702,814,969]
alves_pelog_bem :: Tuning
alves_pelog_bem = Tuning (Left alves_pelog_bem_r) 2

-- > let c = [0,386,471,857,969]
-- > in map (round.to_cents_r) alves_pelog_barang_r == c
alves_pelog_barang_r :: [Rational]
alves_pelog_barang_r = [1,5/4,21/16,105/64,7/4]

-- | HMC /pelog barang/ tuning.
--
-- > cents_i alves_pelog_barang == [0,386,471,857,969]
alves_pelog_barang :: Tuning
alves_pelog_barang = Tuning (Left alves_pelog_barang_r) 2

-- > let c = [0,386,471,702,969]
-- > in map (round.to_cents_r) alves_pelog_23467 == c
alves_pelog_23467_r :: [Rational]
alves_pelog_23467_r = [1,5/4,21/16,3/2,7/4]

-- | HMC /pelog 2,3,4,6,7/ tuning.
--
-- > cents_i alves_pelog_23467 == [0,386,471,702,969]
alves_pelog_23467 :: Tuning
alves_pelog_23467 = Tuning (Left alves_pelog_23467_r) 2
