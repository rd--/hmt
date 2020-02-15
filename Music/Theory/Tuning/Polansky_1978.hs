-- | Larry Polansky. \"Psaltery (for Lou Harrison)\".
-- Frog Peak Music, 1978.
module Music.Theory.Tuning.Polansky_1978 where

import Data.List {- base -}

import qualified Music.Theory.Tuning as T {- hmt -}
import qualified Music.Theory.Tuning.Type as T {- hmt -}

{- | Three interlocking harmonic series on 1:5:3, by Larry Polansky in \"Psaltery\".

> import qualified Music.Theory.Tuning.Scala as T
> scl <- T.scl_load "polansky_ps"
> T.pitch_representations (T.scale_pitches scl) == (0,50)
> 1 : Data.Either.rights (T.scale_pitches scl) == psaltery_r

-}
psaltery_r :: [Rational]
psaltery_r =
    let sq_at n = map (* n) [1..17]
    in concat [sq_at 1,sq_at (5/4),sq_at (3/2)]

{- | 'T.fold_ratio_to_octave'' of 'psaltery'.

> length psaltery_r == 51 && length psaltery_o_r == 21

> psaltery_o_r == [1,65/64,33/32,17/16,35/32,9/8,75/64,39/32
>                 ,5/4,21/16,85/64,11/8,45/32
>                 ,3/2,25/16,51/32,13/8,27/16,55/32,7/4,15/8]

-}
psaltery_o_r :: [Rational]
psaltery_o_r = nub (sort (map T.fold_ratio_to_octave_err psaltery_r))

{- | 'Tuning' derived from 'psaltery_o' with 'octave_ratio' of @2@.

> cents_i psaltery_o == [0,27,53,105,155,204,275,342,386,471,491,551,590
                        ,702,773,807,841,906,938,969,1088]

> let r = [0,1200,1902,2400,2786,3102,3369,3600,3804,3986,4151,4302,4441,4569,4688,4800,4905
          ,386,1586,2288,2786,3173,3488,3755,3986,4190,4373,4538,4688,4827,4955,5075,5186,5291
          ,702,1902,2604,3102,3488,3804,4071,4302,4506,4688,4853,5004,5142,5271,5390,5502]
> in cents_i (T.scale_tuning 0.01 scl) == r

-}
psaltery_o :: T.Tuning
psaltery_o = T.Tuning (Left psaltery_o_r) Nothing
