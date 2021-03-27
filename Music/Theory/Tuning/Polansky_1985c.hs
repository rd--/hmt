-- | Larry Polansky. "Notes on Piano Study #5".
-- _1, The Journal of the Just Intonation Newtork_, 1(4), Autumn 1985.
module Music.Theory.Tuning.Polansky_1985c where

import Music.Theory.Tuning.Type {- hmt -}

-- | The tuning has four octaves, these ratios are per-octave.
ps5_jpr_r :: [[Rational]]
ps5_jpr_r =
    [[1, 21/20, 9/8, 6/5, 5/4,  4/3,   7/5, 3/2, 8/5,  5/3,  7/4, 15/8]
    ,[1, 21/20, 9/8, 6/5, 5/4,  4/3,   7/5, 3/2, 8/5,  5/3,  7/4, 15/8]
    ,[1, 33/32, 9/8, 6/5, 5/4, 21/16, 11/8, 3/2, 8/5, 13/8,  7/4, 15/8]
    ,[1, 21/20, 9/8, 7/6, 5/4,  4/3,  11/8, 3/2, 8/5, 27/16, 7/4, 15/8]]

{- | Four-octave tuning.

> import Data.List.Split

> let r = [[   0,  84, 204, 316, 386, 498, 583, 702, 814, 884, 969,1088]
>         ,[1200,1284,1404,1516,1586,1698,1783,1902,2014,2084,2169,2288]
>         ,[2400,2453,2604,2716,2786,2871,2951,3102,3214,3241,3369,3488]
>         ,[3600,3684,3804,3867,3986,4098,4151,4302,4414,4506,4569,4688]]
> in chunksOf 12 (cents_i ps5_jpr) == r

> let r = [[0,84,204,316,386,498,583,702,814,884,969,1088]
>         ,[0,84,204,316,386,498,583,702,814,884,969,1088]
>         ,[0,53,204,316,386,471,551,702,814,841,969,1088]
>         ,[0,84,204,267,386,498,551,702,814,906,969,1088]]
> chunksOf 12 (map (`mod` 1200) (cents_i ps5_jpr))
-}
ps5_jpr :: Tuning
ps5_jpr =
    let f m n = map (* m) n
        r = concat (zipWith f [1,2,4,8] ps5_jpr_r)
    in Tuning (Left r) (Just (Left 4))
