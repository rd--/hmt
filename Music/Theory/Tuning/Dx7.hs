{- | Yamaha/Dx7

The various Yamaha DX & TX synthesisers have different tuning resolutions.

The "Grey Matter Response E!" tuning tables state the Dx7 internally divides the octave into 4096 parts (steps of 0.293 cents).

The TX817 manual states a resolution of 768 parts per octave (steps of 1.5625 cents)

Others have a resolution of 1024 parts per octave (steps of 1.1719 cents).

Here a "point" is the finest of these, 1/4096 of an octave.

-}
module Music.Theory.Tuning.Dx7 where

-- | Cents to points where there are 4096 equally spaced points per octave.
cents_to_points :: Fractional a => a -> a
cents_to_points c = (c / 1200) * 4096

{- | Integral form of cents_to_points.

>>> map cents_to_points_i [0,600,1200]
[0,2048,4096]
-}
cents_to_points_i :: Integral i => i -> i
cents_to_points_i = round . cents_to_points . fromIntegral

{- | Inverse of cents_to_points.

>>> map points_to_cents [1, 2, 4, 8]
[0.29296875,0.5859375,1.171875,2.34375]
-}
points_to_cents :: Fractional a => a -> a
points_to_cents p = (p / 4096) * 1200

{- | Integral version of points_to_cents.

>>> map points_to_cents_i [0,2048,4096]
[0,600,1200]
-}
points_to_cents_i :: Integral i => i -> i
points_to_cents_i = round . points_to_cents . fromIntegral
