-- | David Lewin. \"A Response to a Response: On PC Set
-- Relatedness\". /Perspectives of New Music/, 18(1-2):498-502, 1980.
module Music.Theory.Z12.Lewin_1980 where

import Data.List
import Music.Theory.Z12
import qualified Music.Theory.Z12.Castren_1994 as C

-- | REL function with given /ncv/ function (see 't_rel' and 'ti_rel').
rel :: Floating n => (Int -> [a] -> [n]) -> [a] -> [a] -> n
rel ncv x y =
    let n = min (genericLength x) (genericLength y)
        p = map (\i -> ncv i x) [2..n]
        q = map (\i -> ncv i y) [2..n]
        f a b = zipWith (\i j -> sqrt (i * j)) a b
        pt = sum (map sum p)
        qt = sum (map sum q)
    in sum (map sum (zipWith f p q)) / sqrt (pt * qt)

-- | T-equivalence REL function.
--
-- Kuusi 2001, 7.5.2
--
-- > let (~=) p q = abs (p - q) < 1e-2
-- > t_rel [0,1,2,3,4] [0,2,3,6,7] ~= 0.44
-- > t_rel [0,1,2,3,4] [0,2,4,6,8] ~= 0.28
-- > t_rel [0,2,3,6,7] [0,2,4,6,8] ~= 0.31
t_rel :: Floating n => [Z12] -> [Z12] -> n
t_rel = rel C.t_n_class_vector

-- | T/I-equivalence REL function.
--
-- Buchler 1998, Fig. 3.38
--
-- > let (~=) p q = abs (p - q) < 1e-3
-- > let a = [0,2,3,5,7]::[Z12]
-- > let b = [0,2,3,4,5,8]::[Z12]
-- > let g = [0,1,2,3,5,6,8,10]::[Z12]
-- > let j = [0,2,3,4,5,6,8]::[Z12]
-- > ti_rel a b ~= 0.593
-- > ti_rel a g ~= 0.648
-- > ti_rel a j ~= 0.509
-- > ti_rel b g ~= 0.712
-- > ti_rel b j ~= 0.892
-- > ti_rel g j ~= 0.707
ti_rel :: Floating n => [Z12] -> [Z12] -> n
ti_rel = rel C.ti_n_class_vector
