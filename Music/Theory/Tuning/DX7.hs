-- * Yamaha/DX7

import Data.Bits {- base -}

cents_to_points :: Fractional a => a -> a
cents_to_points c = (c / 1200) * 4096

-- > map cents_to_points_i [0,600,1200] == [0,2048,4096]
cents_to_points_i :: Integral i => i -> i
cents_to_points_i = round . cents_to_points . fromIntegral

points_to_cents :: Fractional a => a -> a
points_to_cents p = (p / 4096) * 1200

-- > map points_to_cents_i [0,2048,4096] == [0,600,1200]
points_to_cents_i :: Integral i => i -> i
points_to_cents_i = round . points_to_cents . fromIntegral
