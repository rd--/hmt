module Music.Theory.Z.TTO where

import Music.Theory.Z

-- | Twelve-tone operator,of the form TMI.
data TTO t = TTO {tto_T :: t,tto_M :: Bool,tto_I :: Bool}
             deriving (Eq,Show)

tto_pp :: Show t => TTO t -> String
tto_pp o = concat ["T",show (tto_T o),if tto_M o then "M" else "",if tto_I o then "I" else ""]

-- > length (tto_univ 12) == 48
tto_univ :: (Enum t, Num t) => t -> [TTO t]
tto_univ z = [TTO t m i | t <- [0 .. z - 1], m <- [False,True], i <- [False,True]]

-- > tto_apply 5 12 (TTO 1 True False) [0,1,2,3] == [1,6,11,4]
tto_apply :: Integral t => t -> t -> TTO t -> [t] -> [t]
tto_apply mn z (TTO t m i) =
    let i_f = if i then map (z_negate z) else id
        m_f = if m then map (z_mul z mn) else id
        t_f = if t > 0 then map (z_add z t) else id
    in t_f . m_f . i_f
