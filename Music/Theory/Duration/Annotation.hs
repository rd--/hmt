-- | Duration annotations.
module Music.Theory.Duration.Annotation where

import Music.Theory.Duration

-- | Standard music notation durational model annotations
data D_Annotation = Tie_Right
                  | Tie_Left
                  | Begin_Tuplet (Integer,Integer,Duration)
                  | End_Tuplet
                    deriving (Eq,Show)

-- | Annotated 'Duration'.
type Duration_A = (Duration,[D_Annotation])

-- | Is 'Duration_A' tied to the the right?
da_tied_right :: Duration_A -> Bool
da_tied_right = elem Tie_Right . snd

