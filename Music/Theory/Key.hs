-- | Common music keys.
module Music.Theory.Key where

import Control.Monad {- base -}
import Data.Char {- base -}
import Data.List {- base -}

import qualified Music.Theory.Pitch as T
import qualified Music.Theory.Pitch.Name as T
import qualified Music.Theory.Pitch.Note as T
import qualified Music.Theory.Interval as T

-- | Enumeration of common music notation modes.
data Mode_T = Minor_Mode | Major_Mode
              deriving (Eq,Ord,Show)

mode_pp :: Mode_T -> String
mode_pp m =
    case m of
      Minor_Mode -> "Minor"
      Major_Mode -> "Major"

mode_identifier_pp :: Mode_T -> String
mode_identifier_pp = map toLower . mode_pp

-- | There are two modes, given one return the other.
mode_parallel :: Mode_T -> Mode_T
mode_parallel m = if m == Minor_Mode then Major_Mode else Minor_Mode

-- | A common music notation key is a 'Note_T', 'Alteration_T',
-- 'Mode_T' triple.
type Key = (T.Note_T,T.Alteration_T,Mode_T)

key_mode :: Key -> Mode_T
key_mode (_,_,m) = m

-- | Enumeration of 42 CMN keys.
--
-- > length key_sequence_42 == 7 * 3 * 2
key_sequence_42 :: [Key]
key_sequence_42 =
    let a_seq = [T.Flat,T.Natural,T.Sharp]
        m_seq = [Major_Mode,Minor_Mode]
    in [(n,a,m) | n <- T.note_seq,a <- a_seq,m <- m_seq]

-- | Subset of 'key_sequence' not including very eccentric keys (where
-- there are more than 7 alterations).
--
-- > length key_sequence_30 == 30
key_sequence_30 :: [Key]
key_sequence_30 = filter (\k -> maybe False ((< 8) . abs) (key_fifths k)) key_sequence_42

-- | Parallel key, ie. 'mode_parallel' of 'Key'.
key_parallel :: Key -> Key
key_parallel (n,a,m) = (n,a,mode_parallel m)

key_transpose :: Key -> Int -> Key
key_transpose (n,a,m) x =
    let Just pc = T.note_alteration_to_pc (n,a)
        Just (n',a') = T.pc_to_note_alteration_ks ((pc + x) `mod` 12)
    in (n',a',m)

-- | Relative key (ie. 'mode_parallel' with the same number of and type of alterations.
--
-- > let k = [(T.C,T.Natural,Major_Mode),(T.E,T.Natural,Minor_Mode)]
-- > in map (key_lc_uc_pp . key_relative) k == ["a♮","G♮"]
key_relative :: Key -> Key
key_relative k =
    case key_mode k of
      Major_Mode -> key_parallel (key_transpose k 9)
      Minor_Mode -> key_parallel (key_transpose k 3)

-- | Mediant minor of major key.
--
-- > key_mediant (T.C,T.Natural,Major_Mode) == Just (T.E,T.Natural,Minor_Mode)
key_mediant :: Key -> Maybe Key
key_mediant k =
    case key_mode k of
      Major_Mode -> Just (key_parallel (key_transpose k 4))
      _ -> Nothing

key_lc_pp :: (T.Alteration_T -> String) -> Key -> String
key_lc_pp a_pp (n,a,m) =
    let c = T.note_pp n
        c' = if m == Minor_Mode then toLower c else c
    in c' : a_pp a

-- | Pretty-printer where 'Minor_Mode' is written in lower case (lc) and
-- alteration symbol is unicode (uc).
--
-- > map key_lc_uc_pp [(C,Sharp,Minor_Mode),(E,Flat,Major_Mode)] == ["c♯","E♭"]
key_lc_uc_pp :: Key -> String
key_lc_uc_pp = key_lc_pp (return . T.alteration_symbol)

key_lc_iso_pp :: Key -> String
key_lc_iso_pp = key_lc_pp T.alteration_iso

-- > map key_lc_tonh_pp [(T.C,T.Sharp,Minor_Mode),(T.E,T.Flat,Major_Mode)]
key_lc_tonh_pp :: Key -> String
key_lc_tonh_pp = key_lc_pp T.alteration_tonh

-- > map key_identifier_pp [(T.C,T.Sharp,Minor_Mode),(T.E,T.Flat,Major_Mode)]
key_identifier_pp :: (Show a, Show a1) => (a, a1, Mode_T) -> [Char]
key_identifier_pp (n,a,m) = map toLower (intercalate "_" [show n,show a,mode_pp m])

note_char_to_key :: Char -> Maybe Key
note_char_to_key c =
    let m = if isUpper c then Major_Mode else Minor_Mode
    in fmap (\n -> (n,T.Natural,m)) (T.parse_note True c)

-- | Parse 'Key' from /lc-uc/ string.
--
-- > import Data.Maybe
--
-- > let k = mapMaybe key_lc_uc_parse ["c","E","f♯","ab","G#"]
-- > in map key_lc_uc_pp k == ["c♮","E♮","f♯","a♭","G♯"]
key_lc_uc_parse :: String -> Maybe Key
key_lc_uc_parse k =
    let with_k a (n,_,m) = (n,a,m)
        with_a n a = fmap (with_k a) (note_char_to_key n)
    in case k of
         [c] -> note_char_to_key c
         [n,a] -> join (fmap (with_a n) (T.symbol_to_alteration_iso a))
         _ -> Nothing

-- | Distance along circle of fifths path of indicated 'Key'.  A
-- positive number indicates the number of sharps, a negative number
-- the number of flats.
--
-- > key_fifths (A,Natural,Minor_Mode) == Just 0
-- > key_fifths (A,Natural,Major_Mode) == Just 3
-- > key_fifths (C,Natural,Minor_Mode) == Just (-3)
-- > key_fifths (B,Sharp,Minor_Mode) == Just 9
--
-- > zip (map key_lc_iso_pp key_sequence) (map key_fifths key_sequence)
key_fifths :: Key -> Maybe Int
key_fifths (n,a,m) =
    let cf x = let (p,q) = T.circle_of_fifths x in p ++ q
        eq (T.Pitch n' a' _) = n == n' && a == a'
        ix = case m of
               Major_Mode -> findIndex eq (cf T.c4)
               Minor_Mode -> findIndex eq (cf T.a4)
    in fmap (\i -> if i < 13 then negate i else i - 12) ix
