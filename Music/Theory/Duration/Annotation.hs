-- | Duration annotations.
module Music.Theory.Duration.Annotation where

import Data.Ratio
import Music.Theory.Duration
import Music.Theory.Duration.RQ

-- | Standard music notation durational model annotations
data D_Annotation = Tie_Right
                  | Tie_Left
                  | Begin_Tuplet (Integer,Integer,Duration)
                  | End_Tuplet
                    deriving (Eq,Show)

-- | Annotated 'Duration'.
type Duration_A = (Duration,[D_Annotation])

begins_tuplet :: D_Annotation -> Bool
begins_tuplet a =
    case a of
      Begin_Tuplet _ -> True
      _ -> False

-- | Does 'Duration_A' being a tuplet?
da_begins_tuplet :: Duration_A -> Bool
da_begins_tuplet (_,a) = any begins_tuplet a

-- | Is 'Duration_A' tied to the the right?
da_tied_right :: Duration_A -> Bool
da_tied_right = elem Tie_Right . snd

-- | Annotate a sequence of 'Duration_A' as a tuplet.
--
-- > import Music.Theory.Duration.Name
-- > da_tuplet (3,2) [(quarter_note,[Tie_Left]),(eighth_note,[Tie_Right])]
da_tuplet :: (Integer,Integer) -> [Duration_A] -> [Duration_A]
da_tuplet (d,n) x =
    let fn (p,q) = (p {multiplier = n%d},q)
        k = sum (map (duration_to_rq . fst) x) / (d%1)
        ty = rq_to_duration_err (show ("da_tuplet",d,n,x,k)) k
        t0 = [Begin_Tuplet (d,n,ty)]
        ts = [t0] ++ replicate (length x - 2) [] ++ [[End_Tuplet]]
        jn (p,q) z = (p,q++z)
    in zipWith jn (map fn x) ts
