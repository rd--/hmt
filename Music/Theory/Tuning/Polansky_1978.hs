-- | Larry Polansky. \"Psaltery (for Lou Harrison)\". Frog Peak Music,
-- 1978.
module Music.Theory.Tuning.Polansky_1978 where

import Data.List {- base -}

import Music.Theory.Tuning

-- | Three interlocking harmonic series on 1:5:3, by Larry Polansky in
-- \"Psaltery\".
--
-- > import qualified Music.Theory.Tuning.Scala as T
-- > let fn = "/home/rohan/data/scala/83/scl/polansky_ps.scl"
-- > s <- T.load fn
-- > T.scale_pitch_representations s == (0,50)
-- > 1 : Data.Either.rights (T.scale_pitches s) == psaltery_r
psaltery_r :: [Rational]
psaltery_r =
    let sq_at n = map (* n) [1..17]
    in concat [sq_at 1,sq_at (5/4),sq_at (3/2)]

-- | 'T.fold_ratio_to_octave' of 'psaltery'.
--
-- > length psaltery == 51 && length psaltery_o == 21
-- > psaltery_o == [1,65/64,33/32,17/16,35/32,9/8,75/64,39/32
-- >               ,5/4,21/16,85/64,11/8,45/32
-- >               ,3/2,25/16,51/32,13/8,27/16,55/32,7/4,15/8]
psaltery_o :: [Rational]
psaltery_o = nub (sort (map fold_ratio_to_octave psaltery_r))

-- | 'Tuning' derived from 'psaltery_o' with 'octave_ratio' of @2@.
psaltery :: Tuning
psaltery = Tuning (Left psaltery_o) 2

-- Local Variables:
-- truncate-lines:t
-- End:
