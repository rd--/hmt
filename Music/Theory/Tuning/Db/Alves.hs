-- | Bill Alves.
module Music.Theory.Tuning.Db.Alves where

import qualified Music.Theory.Tuning.Type as Tuning {- hmt -}

{- | Ratios for 'harrison_ditone' (SCALA=pyth_12)

>>> import qualified Music.Theory.Tuning as Tuning
>>> map (round . Tuning.ratio_to_cents) harrison_ditone_r
[0,114,204,294,408,498,612,702,816,906,996,1110]

>>> import qualified Music.Theory.Tuning.Scala as Scala
>>> db <- Scala.scl_load_db_dir
>>> let scl = Scala.scl_find_ji True (==) (harrison_ditone_r ++ [2]) db
>>> map Scala.scale_name scl
["pyth_12"]

>>> map Scala.scale_description scl
["12-tone Pythagorean scale"]
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

>>> Tuning.tn_divisions harrison_ditone
12

>>> Tuning.tn_cents_i harrison_ditone
[0,114,204,294,408,498,612,702,816,906,996,1110]
-}
harrison_ditone :: Tuning.Tuning
harrison_ditone = Tuning.Tuning (Left harrison_ditone_r) Nothing
