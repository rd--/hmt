-- | Larry Polansky. \"Psaltery (for Lou Harrison)\". Frog Peak Music,
-- 1978.
module Music.Theory.Tuning.Polansky_1978 where

import Data.List
import qualified Music.Theory.Tuning as T

-- | Three interlocking harmonic series on 1:5:3, by Larry Polansky in
-- \"Psaltery\".
--
-- > import qualified Music.Theory.Tuning.Scala as T
-- > let fn = "/home/rohan/opt/scala/scl/polansky_ps.scl"
-- > s <- T.load fn
-- > T.scale_pitch_representations s == (0,50)
-- > 1 : Data.Either.rights (T.scale_pitches s) == psaltery
psaltery :: [Rational]
psaltery = [1,2/1,3/1,4/1,5/1,6/1,7/1,8/1,9/1,10/1,11/1,12/1,13/1,14/1,15/1,16/1,17/1,5/4,5/2,15/4,5/1,25/4,15/2,35/4,10/1,45/4,25/2,55/4,15/1,65/4,35/2,75/4,20/1,85/4,3/2,3/1,9/2,6/1,15/2,9/1,21/2,12/1,27/2,15/1,33/2,18/1,39/2,21/1,45/2,24/1,51/2]

-- | 'T.fold_to_octave' of 'psaltery'.
--
-- > length psaltery == 51 && length psaltery_o == 21
-- > psaltery_o == [1,65/64,33/32,17/16,35/32,9/8,75/64,39/32,5/4,21/16,85/64,11/8,45/32,3/2,25/16,51/32,13/8,27/16,55/32,7/4,15/8]
psaltery_o :: [Rational]
psaltery_o = nub (sort (map T.fold_to_octave psaltery))
