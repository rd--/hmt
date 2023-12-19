-- | Bill Alves.
module Music.Theory.Tuning.Db.Alves where

import Music.Theory.Tuning.Type {- hmt -}

{- | Ratios for 'harrison_ditone' (SCALA=pyth_12)

> import Music.Theory.Tuning
> let c = [0,114,204,294,408,498,612,702,816,906,996,1110]
> map (round . ratio_to_cents) harrison_ditone_r == c

> import Music.Theory.Tuning.Scala
> scl_find_ji (harrison_ditone_r ++ [2])
-}
harrison_ditone_r :: [Rational]
harrison_ditone_r =
  [ 1
  , 2187 / 2048 {- 256/243 -}
  , 9 / 8
  , 32 / 27
  , 81 / 64
  , 4 / 3
  , 729 / 512
  , 3 / 2
  , 6561 / 4096 {- 128/81 -}
  , 27 / 16
  , 16 / 9
  , 243 / 128
  ]

{- | Ditone/pythagorean tuning, <http://www.billalves.com/porgitaro/ditonesettuning.html>

> tn_divisions harrison_ditone == 12
> tn_cents_i harrison_ditone == [0,114,204,294,408,498,612,702,816,906,996,1110]
-}
harrison_ditone :: Tuning
harrison_ditone = Tuning (Left harrison_ditone_r) Nothing
