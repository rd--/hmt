-- | Terry Riley.
module Music.Theory.Tuning.Riley where

import Music.Theory.Tuning {- hmt -}

-- | Ratios for 'riley_albion'.
--
-- > let r = [0,112,204,316,386,498,610,702,814,884,996,1088]
-- > in map (round . ratio_to_cents) riley_albion_r == r
riley_albion_r :: [Rational]
riley_albion_r = [1/1,16/15,9/8,6/5,5/4,4/3,64/45,3/2,8/5,5/3,16/9,15/8]

-- | Riley's five-limit tuning as used in _The Harp of New Albion_,
-- see <http://www.ex-tempore.org/Volx1/hudson/hudson.htm>.
--
-- > cents_i riley_albion == [0,112,204,316,386,498,610,702,814,884,996,1088]
riley_albion :: Tuning
riley_albion = Tuning (Left riley_albion_r) 2
