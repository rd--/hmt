module Music.Theory.Key where

import Data.List
import Music.Theory.Pitch
import Music.Theory.Pitch.Name
import Music.Theory.Interval

data Mode_T = Minor_Mode | Major_Mode
              deriving (Eq,Ord,Show)

key_fifths :: (Note_T,Alteration_T,Mode_T) -> Int
key_fifths (n,a,m) =
    let cf x = let (p,q) = circle_of_fifths x in p ++ q
        eq (Pitch n' a' _) = n == n' && a == a'
        (Just ix) = case m of
                      Major_Mode -> findIndex eq (cf c4)
                      Minor_Mode -> findIndex eq (cf a4)
    in if ix < 13 then negate ix else ix - 12
