{- | Robert Morris. /Composition with Pitch-Classes: A Theory of
Compositional Design/. Yale University Press, New Haven, 1987.
-}
module Music.Theory.Z.Morris_1987 where

import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Math.Z as Z {- hmt-base -}

{- | @INT@ operator.

>>> map (int Z.z12) [[0,1,3,6,10],[3,7,0]]
[[1,2,3,4],[4,5]]
-}
int :: Integral i => Z.Z i -> [i] -> [i]
int z = List.d_dx_by (Z.z_sub z)
