-- | Common music keys.
module Music.Theory.Key where

import Data.List
import Music.Theory.Pitch
import Music.Theory.Pitch.Name
import Music.Theory.Interval

-- | Enumeration of common music notation modes.
data Mode_T = Minor_Mode | Major_Mode
              deriving (Eq,Ord,Show)

-- | A common music notation key is a 'Note_T', 'Alteration_T',
-- 'Mode_T' triple.
type Key = (Note_T,Alteration_T,Mode_T)

-- | Distance along circle of fifths path of indicated 'Key'.  A
-- positive number indicates the number of sharps, a negative number
-- the number of flats.
--
-- > key_fifths (A,Natural,Minor_Mode) == 0
-- > key_fifths (A,Natural,Major_Mode) == 3
-- > key_fifths (C,Natural,Minor_Mode) == -3
key_fifths :: Key -> Int
key_fifths (n,a,m) =
    let cf x = let (p,q) = circle_of_fifths x in p ++ q
        eq (Pitch n' a' _) = n == n' && a == a'
        (Just ix) = case m of
                      Major_Mode -> findIndex eq (cf c4)
                      Minor_Mode -> findIndex eq (cf a4)
    in if ix < 13 then negate ix else ix - 12
