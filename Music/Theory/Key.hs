-- | Common music keys.
module Music.Theory.Key where

import Control.Monad {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Music.Theory.Pitch as Pitch
import qualified Music.Theory.Pitch.Name as Pitch.Name
import qualified Music.Theory.Pitch.Note as Pitch.Note
import qualified Music.Theory.Interval as Interval

-- | Enumeration of common music notation modes.
data Mode = Minor_Mode | Major_Mode
              deriving (Eq,Ord,Show)

-- | Pretty printer for 'Mode'.
mode_pp :: Mode -> String
mode_pp m =
    case m of
      Minor_Mode -> "Minor"
      Major_Mode -> "Major"

-- | Lower-cased 'mode_pp'.
mode_identifier_pp :: Mode -> String
mode_identifier_pp = map toLower . mode_pp

-- | There are two modes, given one return the other.
mode_parallel :: Mode -> Mode
mode_parallel m = if m == Minor_Mode then Major_Mode else Minor_Mode

mode_pc_seq :: Num t => Mode -> [t]
mode_pc_seq md =
    case md of
      Major_Mode -> [0,2,4,5,7,9,11]
      Minor_Mode -> [0,2,3,5,7,8,10]

-- | A common music notation key is a 'Note', 'Alteration', 'Mode' triple.
type Key = (Pitch.Note.Note,Pitch.Note.Alteration,Mode)

-- | 'Mode' of 'Key'.
key_mode :: Key -> Mode
key_mode (_,_,m) = m

{- | Enumeration of 42 CMN keys.

>>> length key_sequence_42 == 7 * 3 * 2
True
-}
key_sequence_42 :: [Key]
key_sequence_42 =
    let a_seq = [Pitch.Note.Flat,Pitch.Note.Natural,Pitch.Note.Sharp]
        m_seq = [Major_Mode,Minor_Mode]
    in [(n,a,m) | n <- Pitch.Note.note_seq,a <- a_seq,m <- m_seq]

{- | Subset of 'key_sequence' not including very eccentric keys (where there are more than 7 alterations).

>>> length key_sequence_30
30
-}
key_sequence_30 :: [Key]
key_sequence_30 = filter (maybe False ((< 8) . abs) . key_fifths) key_sequence_42

-- | Parallel key, ie. 'mode_parallel' of 'Key'.
key_parallel :: Key -> Key
key_parallel (n,a,m) = (n,a,mode_parallel m)

-- | Transposition of 'Key'.
key_transpose :: Key -> Int -> Key
key_transpose (n,a,m) x =
    let pc = fromMaybe (error "key_transpose?") (Pitch.Note.note_alteration_to_pc (n,a))
        (n',a') = fromMaybe (error "key_transpose?") (Pitch.Note.pc_to_note_alteration_ks ((pc + x) `mod` 12))
    in (n',a',m)

{- | Relative key (ie. 'mode_parallel' with the same number of and type of alterations.

>>> let k = [(Pitch.Note.C,Pitch.Note.Natural,Major_Mode),(Pitch.Note.E,Pitch.Note.Natural,Minor_Mode)]
>>> map (key_lc_uc_pp . key_relative) k == ["a♮","G♮"]
True
-}
key_relative :: Key -> Key
key_relative k =
    case key_mode k of
      Major_Mode -> key_parallel (key_transpose k 9)
      Minor_Mode -> key_parallel (key_transpose k 3)

{- | Mediant minor of major key.

>>> key_mediant (Pitch.Note.C,Pitch.Note.Natural,Major_Mode)
Just (E,Natural,Minor_Mode)
-}
key_mediant :: Key -> Maybe Key
key_mediant k =
    case key_mode k of
      Major_Mode -> Just (key_parallel (key_transpose k 4))
      _ -> Nothing

{- | Key pitch class set

>>> fmap key_pc_set (key_lc_uc_parse "E")
Just [1,3,4,6,8,9,11]
-}
key_pc_set :: Integral i => Key -> [i]
key_pc_set (n,a,md) =
    let pc0 = Pitch.Note.note_to_pc n + Pitch.Note.alteration_to_diff_err a
    in sort (map ((`mod` 12) . (+ pc0)) (mode_pc_seq md))

-- | Pretty-printer where 'Minor_Mode' is written in lower case (lc) and
-- alteration symbol is shown using indicated function.
key_lc_pp :: (Pitch.Note.Alteration -> String) -> Key -> String
key_lc_pp a_pp (n,a,m) =
    let c = Pitch.Note.note_pp n
        c' = if m == Minor_Mode then toLower c else c
    in c' : a_pp a

{- | 'key_lc_pp' with unicode (uc) alteration.

>>> map key_lc_uc_pp [(Pitch.Note.C,Pitch.Note.Sharp,Minor_Mode),(Pitch.Note.E,Pitch.Note.Flat,Major_Mode)] == ["c♯","E♭"]
True
-}
key_lc_uc_pp :: Key -> String
key_lc_uc_pp = key_lc_pp (return . Pitch.Note.alteration_symbol)

-- | 'key_lc_pp' with ISO alteration.
key_lc_iso_pp :: Key -> String
key_lc_iso_pp = key_lc_pp Pitch.Note.alteration_iso

{- | 'key_lc_pp' with tonh alteration.

>>> map key_lc_tonh_pp [(Pitch.Note.C,Pitch.Note.Sharp,Minor_Mode),(Pitch.Note.E,Pitch.Note.Flat,Major_Mode)]
["cis","Ees"]
-}
key_lc_tonh_pp :: Key -> String
key_lc_tonh_pp = key_lc_pp Pitch.Note.alteration_tonh

{- | Pretty printer

>>> map key_identifier_pp [(Pitch.Note.C,Pitch.Note.Sharp,Minor_Mode),(Pitch.Note.E,Pitch.Note.Flat,Major_Mode)]
["c_sharp_minor","e_flat_major"]
-}
key_identifier_pp :: (Show a, Show a1) => (a, a1, Mode) -> [Char]
key_identifier_pp (n,a,m) = map toLower (intercalate "_" [show n,show a,mode_pp m])

{- | Note name to key

>>> import Data.Maybe
>>> mapMaybe note_char_to_key "CdEfGaB"
[(C,Natural,Major_Mode),(D,Natural,Minor_Mode),(E,Natural,Major_Mode),(F,Natural,Minor_Mode),(G,Natural,Major_Mode),(A,Natural,Minor_Mode),(B,Natural,Major_Mode)]
-}
note_char_to_key :: Char -> Maybe Key
note_char_to_key c =
    let m = if isUpper c then Major_Mode else Minor_Mode
    in fmap (\n -> (n,Pitch.Note.Natural,m)) (Pitch.Note.parse_note_t True c)

{- | Parse 'Key' from /lc-uc/ string.

>>> let k = mapMaybe key_lc_uc_parse ["c","E","f♯","ab","G#"]
>>> map key_lc_uc_pp k == ["c♮","E♮","f♯","a♭","G♯"]
True
-}
key_lc_uc_parse :: String -> Maybe Key
key_lc_uc_parse k =
    let with_k a (n,_,m) = (n,a,m)
        with_a n a = fmap (with_k a) (note_char_to_key n)
    in case k of
         [c] -> note_char_to_key c
         [n,a] -> with_a n =<< Pitch.Note.symbol_to_alteration_unicode_plus_iso a
         _ -> Nothing

{- | Distance along circle of fifths path of indicated 'Key'.
A positive number indicates the number of sharps, a negative the number of flats.

>>> key_fifths (Pitch.Note.A,Pitch.Note.Natural,Minor_Mode)
Just 0

>>> key_fifths (Pitch.Note.A,Pitch.Note.Natural,Major_Mode)
Just 3

>>> key_fifths (Pitch.Note.C,Pitch.Note.Natural,Minor_Mode)
Just (-3)

>>> key_fifths (Pitch.Note.B,Pitch.Note.Sharp,Minor_Mode)
Just 9

>>> key_fifths (Pitch.Note.E,Pitch.Note.Sharp,Major_Mode)
Just 11

>>> key_fifths (Pitch.Note.B,Pitch.Note.Sharp,Major_Mode)
Nothing

>>> take 6 (zip (map key_lc_iso_pp key_sequence_42) (map key_fifths key_sequence_42))
[("Cb",Just (-7)),("cb",Just (-10)),("C",Just 0),("c",Just (-3)),("C#",Just 7),("c#",Just 4)]
-}
key_fifths :: Key -> Maybe Int
key_fifths (n,a,m) =
    let cf x = let (p,q) = Interval.circle_of_fifths x in p ++ q
        eq (Pitch.Pitch n' a' _) = n == n' && a == a'
        ix = case m of
               Major_Mode -> findIndex eq (cf Pitch.Name.c4)
               Minor_Mode -> findIndex eq (cf Pitch.Name.a4)
    in fmap (\i -> if i < 13 then negate i else i - 12) ix

-- | Table mapping 'Key' to 'key_fifths' value.
key_fifths_tbl :: [(Key,Int)]
key_fifths_tbl =
    let f (k,n) = fmap (\n' -> (k,n')) n
    in mapMaybe f (zip key_sequence_42 (map key_fifths key_sequence_42))

{- | Lookup 'key_fifths' value in 'key_fifths_tbl'.

>>> let a = [0,1,-1,2,-2,3,-3,4,-4,5,-5]
>>> let f md = map key_lc_iso_pp . mapMaybe (fifths_to_key md)
>>> f Minor_Mode a
["a","e","d","b","g","f#","c","c#","f","g#","bb"]

>>> f Major_Mode a
["C","G","F","D","Bb","A","Eb","E","Ab","B","Db"]
-}
fifths_to_key :: Mode -> Int -> Maybe Key
fifths_to_key md n =
    let eq_f = (\((_,_,md'),n') -> md == md' && n == n')
    in fmap fst (find eq_f key_fifths_tbl)

{- | Given sorted pitch-class set, find simplest implied key in given mode.

>>> mapMaybe (implied_key Major_Mode) [[0,2,4],[1,3],[4,10],[3,9],[8,9]]
[(C,Natural,Major_Mode),(E,Natural,Major_Mode),(F,Natural,Major_Mode),(B,Flat,Major_Mode),(A,Natural,Major_Mode)]

>>> map (implied_key Major_Mode) [[0,1,2],[0,1,3,4]]
[Nothing,Nothing]
-}
implied_key :: Integral i => Mode -> [i] -> Maybe Key
implied_key md pc_set =
    let a_seq = [0,1,-1,2,-2,3,-3,4,-4,5,-5,6,-6]
        key_seq = mapMaybe (fifths_to_key md) a_seq
    in find (\k -> pc_set `List.is_subset` key_pc_set k) key_seq

-- | 'key_fifths' of 'implied_key'.
implied_fifths :: Integral i => Mode -> [i] -> Maybe Int
implied_fifths md = key_fifths <=< implied_key md

implied_key_err :: Integral i => Mode -> [i] -> Key
implied_key_err md = fromMaybe (error "implied_key") . implied_key md

implied_fifths_err :: Integral i => Mode -> [i] -> Int
implied_fifths_err md = fromMaybe (error "implied_fifths") . key_fifths . implied_key_err md
