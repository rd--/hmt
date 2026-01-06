{- | Amplitude related functions -}
module Music.Theory.Amplitude where

{- | <http://www.csounds.com/manual/html/ampmidid.html>

> import Sound.Sc3.Plot
> plot_p1_ln [map (ampmidid 20) [0 .. 127],map (ampmidid 60) [0 .. 127]]
-}
ampmidid :: Floating a => a -> a -> a
ampmidid db v =
  let r = 10 ** (db / 20)
      b = 127 / (126 * sqrt r) - 1 / 126
      m = (1 - b) / 127
  in (m * v + b) ** 2

{- | JMcC (Sc3) equation.

>>> map (round . amp_db) [0.125, 0.25, 0.5, 1]
[-18,-12,-6,0]

> plot_p1_ln [map amp_db [0,0.005 .. 1]]
-}
amp_db :: Floating a => a -> a
amp_db a = logBase 10 a * 20

{- | JMcC (Sc3) equation.

>>> map (Music.Theory.Math.round_to 0.125 . db_amp) [-18,-12,-6,0]
[0.125,0.25,0.5,1.0]

> plot_p1_ln [map db_amp [-60,-59 .. 0]]
-}
db_amp :: Floating a => a -> a
db_amp a = 10 ** (a * 0.05)
