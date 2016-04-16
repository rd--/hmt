-- | Common music keys.
module Music.Theory.Key where

import Control.Monad {- base -}
import Data.Char {- base -}
import Data.List {- base -}

import Music.Theory.Pitch
import Music.Theory.Pitch.Name
import Music.Theory.Pitch.Note
import Music.Theory.Interval

-- | Enumeration of common music notation modes.
data Mode_T = Minor_Mode | Major_Mode
              deriving (Eq,Ord,Show)

-- | There are two modes, given one return the other.
mode_parallel :: Mode_T -> Mode_T
mode_parallel m = if m == Minor_Mode then Major_Mode else Minor_Mode

-- | A common music notation key is a 'Note_T', 'Alteration_T',
-- 'Mode_T' triple.
type Key = (Note_T,Alteration_T,Mode_T)

-- | Enumeration of 42 CMN keys.
--
-- > length key_sequence == 7 * 3 * 2
key_sequence :: [Key]
key_sequence = [(n,a,m) | n <- [C .. B],a <- [Flat,Natural,Sharp],m <- [Major_Mode,Minor_Mode]]

-- | Subset of 'key_sequence' not including very eccentric keys (where
-- there are more than 7 alterations).
--
-- > length key_sequence_7 == 30
key_sequence_7 :: [Key]
key_sequence_7 = filter (\k -> maybe False ((< 8) . abs) (key_fifths k)) key_sequence

-- | Parallel key, ie. 'mode_parallel' of 'Key'.
key_parallel :: Key -> Key
key_parallel (n,a,m) = (n,a,mode_parallel m)

-- | Relative key (ie. 'mode_parallel' with the same number of and type of alterations.
--
-- > let k = [(C,Natural,Major_Mode),(E,Natural,Minor_Mode)]
-- > in map (key_lc_uc_pp . key_relative) k == ["a♮","G♮"]
key_relative :: Key -> Key
key_relative (n,a,m) =
    case m of
      Major_Mode -> (note_t_transpose n 5,a,Minor_Mode)
      Minor_Mode -> (note_t_transpose n 2,a,Major_Mode)

key_lc_pp :: (Alteration_T -> String) -> Key -> String
key_lc_pp a_pp (n,a,m) =
    let c = note_pp n
        c' = if m == Minor_Mode then toLower c else c
    in c' : a_pp a

-- | Pretty-printer where 'Minor_Mode' is written in lower case (lc) and
-- alteration symbol is unicode (uc).
--
-- > map key_lc_uc_pp [(C,Sharp,Minor_Mode),(E,Flat,Major_Mode)] == ["c♯","E♭"]
key_lc_uc_pp :: Key -> String
key_lc_uc_pp = key_lc_pp (return . alteration_symbol)

key_lc_iso_pp :: Key -> String
key_lc_iso_pp = key_lc_pp alteration_iso

note_char_to_key :: Char -> Maybe Key
note_char_to_key c =
    let m = if isUpper c then Major_Mode else Minor_Mode
    in fmap (\n -> (n,Natural,m)) (parse_note True c)

-- | Parse 'Key' from /lc-uc/ string.
--
-- > import Data.Maybe
--
-- > let k = mapMaybe key_lc_uc_parse ["c","E","f♯"]
-- > in map key_lc_uc_pp k == ["c♮","E♮","f♯"]
key_lc_uc_parse :: String -> Maybe Key
key_lc_uc_parse k =
    let with_k a (n,_,m) = (n,a,m)
        with_a n a = fmap (with_k a) (note_char_to_key n)
    in case k of
         [c] -> note_char_to_key c
         [n,a] -> join (fmap (with_a n) (symbol_to_alteration a))
         _ -> Nothing

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
