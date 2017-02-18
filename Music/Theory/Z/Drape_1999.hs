module Music.Theory.Z.Drape_1999 where

import Music.Theory.Z
import Music.Theory.Z.SRO
import Music.Theory.Z.TTO

{- | Relate sets (TnMI).

>>> $ pct rs 0123 641B
>>> T1M

> map tto_pp (rs 5 mod12 [0,1,2,3] [6,4,1,11]) == ["T1M","T4MI"]
-}
rs :: Integral t => t -> Z t -> [t] -> [t] -> [TTO t]
rs = z_tto_rel

{- | Relate segments.

>>> $ pct rsg 156 3BA
>>> T4I
>>> $ pct rsg 0123 05A3
>>> T0M
>>> $ pct rsg 0123 4B61
>>> RT1M
>>> $ pct rsg 0123 B614
>>> r3RT1M

> let sros = map sro_parse . words
> rsg 5 mod12 [1,5,6] [3,11,10] == sros "T4I r1RT4MI"
> rsg 5 mod12 [0,1,2,3] [0,5,10,3] == sros "T0M RT3MI"
> rsg 5 mod12 [0,1,2,3] [4,11,6,1] == sros "T4MI RT1M"
> rsg 5 mod12 [0,1,2,3] [11,6,1,4] == sros "r1T4MI r1RT1M"

-}
rsg :: Integral i => i -> Z i -> [i] -> [i] -> [SRO i]
rsg m z x y = filter (\o -> z_sro_apply m z o x == y) (z_sro_univ (length x) z)
