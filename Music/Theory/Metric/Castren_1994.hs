-- | Marcus Castrén. \"RECREL: A Similarity Measure for
-- Set-Classes\". PhD thesis, Sibelius Academy, Helsinki, 1994.
module Music.Theory.Metric.Castren_1994 where

import Data.Ratio
import qualified Music.Theory.PitchClass as T

-- > dyad_class_percentage_vector [0,1,2,3,4] == [40,30,20,10,0,0]
-- > dyad_class_percentage_vector [0,1,4,5,7] == [20,10,20,20,20,10]
dyad_class_percentage_vector :: Integral b => [b] -> [b]
dyad_class_percentage_vector p =
    let p' = T.icv p
    in map ((*) (sum p')) p'

-- > rel [0,1,2,3,4] [0,1,4,5,7] == 40
-- > rel [0,1,2,3,4] [0,2,4,6,8] == 60
-- > rel [0,1,4,5,7] [0,2,4,6,8] == 60
rel :: Integral a => [a] -> [a] -> Ratio a
rel x y =
    let x' = dyad_class_percentage_vector x
        y' = dyad_class_percentage_vector y
    in sum (map abs (zipWith (-) x' y')) % 2
