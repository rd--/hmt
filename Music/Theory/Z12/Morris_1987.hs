-- | Robert Morris. /Composition with Pitch-Classes: A Theory of
-- Compositional Design/. Yale University Press, New Haven, 1987.
module Music.Theory.Z12.Morris_1987 where

import Music.Theory.List
import Music.Theory.Z12

-- | @INT@ operator.
--
-- > int [0,1,3,6,10] == [1,2,3,4]
int :: [Z12] -> [Z12]
int = d_dx
