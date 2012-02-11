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

-- | Does 'Duration_A' end a tuplet?
da_ends_tuplet :: Duration_A -> Bool
da_ends_tuplet (_,a) = any (== End_Tuplet) a

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

-- | Group tuplets.  Note that this does /not/ handle nested tuplets.
--
-- > import Music.Theory.Duration.Name.Abbreviation
-- > let d = [(q,[]),(e,[Begin_Tuplet (3,2,q)]),(q,[End_Tuplet]),(q,[])]
-- > in da_group_tuplets d
da_group_tuplets :: [Duration_A] -> [Either Duration_A [Duration_A]]
da_group_tuplets x =
    case x of
      [] -> []
      d:x' -> if da_begins_tuplet d
              then let (t,x'') = span da_ends_tuplet x'
                   in Right (d : t) : da_group_tuplets x''
              else Left d : da_group_tuplets x'
