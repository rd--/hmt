{- | Robert Morris. /Composition with Pitch-Classes: A Theory of
Compositional Design/. Yale University Press, New Haven, 1987.
-}
module Music.Theory.Z.Morris_1987 where

import Music.Theory.List {- hmt -}
import Music.Theory.Z {- hmt -}

{- | @INT@ operator.

> map (int z12) [[0,1,3,6,10],[3,7,0]] == [[1,2,3,4],[4,5]]
-}
int :: Integral i => Z i -> [i] -> [i]
int z = d_dx_by (z_sub z)
