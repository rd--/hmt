module Music.Theory.Z.TTO where

-- | Twelve-tone operator,of the form TMI.
data TTO t = TTO {tto_T :: t,tto_M :: Bool,tto_I :: Bool}
             deriving (Eq,Show)

tto_pp :: Show t => TTO t -> String
tto_pp o = concat ["T",show (tto_T o),if tto_M o then "M" else "",if tto_I o then "I" else ""]
