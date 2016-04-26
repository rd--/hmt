module Music.Theory.Z.Drape_1999 where

import qualified Music.Theory.Set.List as T
import Music.Theory.Z
import Music.Theory.Z.SRO
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

{- | Relate segments.

> rsg 5 mod12 [1,5,6] [3,11,10] == [sro_parse "T4I",sro_parse "r1RT4MI"]
> rsg 5 mod12 [0,1,2,3] [0,5,10,3] == [sro_parse "T0M",sro_parse "RT3MI"]
> rsg 5 mod12 [0,1,2,3] [4,11,6,1] == [sro_parse "T4MI",sro_parse "RT1M"]
> rsg 5 mod12 [0,1,2,3] [11,6,1,4] == [sro_parse "r1T4MI",sro_parse "r1RT1M"]

-}
rsg :: Integral i => i -> Z i -> [i] -> [i] -> [SRO i]
rsg m z x y = filter (\o -> z_sro_apply m z o x == y) (z_sro_univ (length x) z)
