module Music.Theory.Z.Drape_1999 where

import qualified Music.Theory.Set.List as T
import Music.Theory.Z
import Music.Theory.Z.TTO

-- | Relate sets (TnMI).
--
-- > let r = [("T1M",[1,6,11,4]),("T4MI",[4,11,6,1])]
-- > map (\(o,s) -> (tto_pp o,s)) (rs 5 mod12 [0,1,2,3] [6,4,1,11]) == r
rs :: (Eq t,Ord t,Enum t,Integral t) => t -> Z t -> [t] -> [t] -> [(TTO t, [t])]
rs m z x y =
    let xs = map (\o -> (o,z_tto_apply m z o x)) (z_tto_univ z)
        q = T.set y
    in filter (\(_,p) -> T.set p == q) xs
