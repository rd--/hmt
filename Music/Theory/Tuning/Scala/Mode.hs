{- | Parser for the @modename.par@ file.

The terminology here is:

- a mode is a subset of the notes of a tuning system (which in scala is called a scale)

- the length (or degree) of the mode is the number of tones in the mode

- the universe (or scale) of the mode is the number of tones in the
  tuning system (or scale) the mode is a subset of
-}
module Music.Theory.Tuning.Scala.Mode where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Function as Function {- hmt -}
import qualified Music.Theory.List as List {- hmt -}
import qualified Music.Theory.Tuning.Scala as Scala {- hmt -}

-- | (mode-start-degree,mode-intervals,mode-description)
type Mode = (Int, [Int], String)

{- | Starting degree of mode in underlying scale.  If non-zero the
mode will not lie within an ordinary octave of the tuning.
-}
mode_starting_degree :: Mode -> Int
mode_starting_degree (d, _, _) = d

-- | Intervals (in steps) between adjacent elements of the mode.
mode_intervals :: Mode -> [Int]
mode_intervals (_, i, _) = i

-- | Interval set of mode (ie. 'nub' of 'sort' of 'mode_intervals')
mode_iset :: Mode -> [Int]
mode_iset = nub . sort . mode_intervals

-- | Histogram ('List.histogram') of 'mode_intervals'
mode_histogram :: Mode -> [(Int, Int)]
mode_histogram = List.histogram . mode_intervals

-- | The text description of the mode, ordinarily a comma separated list of names.
mode_description :: Mode -> String
mode_description (_, _, d) = d

-- | 'length' (or degree) of 'mode_intervals' (ie. number of notes in mode)
mode_length :: Mode -> Int
mode_length = length . mode_intervals

-- | 'sum' of 'mode_intervals' (ie. number of notes in tuning system)
mode_univ :: Mode -> Int
mode_univ = sum . mode_intervals

-- | 'List.dx_d' of 'mode_intervals'.  This seqence includes the octave.
mode_degree_seq :: Mode -> [Int]
mode_degree_seq = List.dx_d 0 . mode_intervals

-- | (mode-count,mode-length-maxima,mode-list)
type ModeNam = (Int, Int, [Mode])

modenam_modes :: ModeNam -> [Mode]
modenam_modes (_, _, m) = m

-- | Search for mode by interval list.
modenam_search_seq :: ModeNam -> [Int] -> [Mode]
modenam_search_seq (_, _, m) x = filter ((== x) . mode_intervals) m

{- | Expect /one/ result.

>>> mn <- load_modenam
>>> let sq = take 90 . mode_description . fromJust . modenam_search_seq1 mn
>>> sq [2,2,1,2,2,2,1]
"G.Lydian, M.Ionian, M.Hypolydian, Major, Bilaval That, Mela Shankarabharanam, Raga Atana, "

>>> sq [2,1,2,2,1,2,2]
"G.M.Hypodorian, G.M.Aeolian, G.Hyperphrygian, Natural Minor, Melodic Minor descending, Asa"

>>> sq [2,1,2,2,1,3,1]
"Harmonic Minor, Mischung 4, Pilu That, Mela Kiravani, Raga Kiranavali, Kirvani (Kirwani), "

>>> sq (replicate 6 2)
"Whole-tone, Messiaen mode 1, Raga Gopriya, Anhemitonic Hexatonic"

>>> sq [1,2,1,2,1,2,1,2]
"Octatonic, Messiaen mode 2, Dominant Diminished, Diminished Blues, Half-Whole step scale"

>>> sq [2,1,2,1,2,1,2,1]
"Diminished, Modus conjunctus, Messiaen mode 2 inverse, Whole-Half step scale"

>>> sq (replicate 12 1)
"Twelve-tone Chromatic (1/11-comma)"
-}
modenam_search_seq1 :: ModeNam -> [Int] -> Maybe Mode
modenam_search_seq1 mn = List.unlist1 . modenam_search_seq mn

{- | Search for mode by description text.

>>> mn <- load_modenam
>>> map (length . map mode_intervals . modenam_search_description mn) ["Messiaen","Xenakis","Raga"]
[24,3,339]
-}
modenam_search_description :: ModeNam -> String -> [Mode]
modenam_search_description (_, _, m) x = filter (isInfixOf x . mode_description) m

-- | Is /p/ an element of the set of rotations of /q/.
mode_rot_eqv :: Mode -> Mode -> Bool
mode_rot_eqv p q =
  (mode_length p == mode_length q)
    && (mode_univ p == mode_univ q)
    && (mode_intervals p `elem` List.rotations (mode_intervals q))

{- | Pretty printer.

>>> mn <- load_modenam

>>> let r = filter ((/=) 0 . mode_starting_degree) (modenam_modes mn) -- non-zero starting degrees
>>> length r
17

>>> let r = filter ((== [(1,2),(2,5)]) . mode_histogram) (modenam_modes mn) -- 2×1 and 5×2
>>> length r
19

>>> let r = filter ((== 22) . mode_univ) (modenam_search_description mn "Raga") -- raga of 22 shruti univ
>>> length r
44

>>> let eq = [(p,q) | p <- r, q <- r, p < q, mode_rot_eqv p q] -- rotationally equivalent elements of r
>>> length eq
8

> putStrLn $ unlines $ intercalate ["\n"] $ map mode_stat r
-}
mode_stat :: Mode -> [String]
mode_stat m =
  let hst = mode_histogram m
      comma_map f = intercalate "," . map f
  in [ "mode-start-degree : " ++ show (mode_starting_degree m)
     , "mode-intervals    : " ++ comma_map show (mode_intervals m)
     , "mode-description  : " ++ mode_description m
     , "mode-length       : " ++ show (mode_length m)
     , "mode-univ         : " ++ show (mode_univ m)
     , "mode-interval-set : " ++ intercalate "," (map show (mode_iset m))
     , "mode-histogram    : " ++ intercalate "," (map (\(e, n) -> concat [show n, "×", show e]) hst)
     , "mode-degree-seq   : " ++ comma_map show (mode_degree_seq m)
     ]

-- * Parser

{- | Bracketed integers are a non-implicit starting degree.

>>> map non_implicit_degree ["4","[4]"]
[Nothing,Just 4]
-}
non_implicit_degree :: String -> Maybe Int
non_implicit_degree s =
  case List.unbracket s of
    Just ('[', x, ']') -> Just (read x)
    _ -> Nothing

-- | Predicate form
is_non_implicit_degree :: String -> Bool
is_non_implicit_degree = isJust . non_implicit_degree

is_integer :: String -> Bool
is_integer = all isDigit

parse_modenam_entry :: [String] -> Mode
parse_modenam_entry w =
  let (n, c) = span (Function.predicate_or is_non_implicit_degree is_integer) w
  in case non_implicit_degree (n !! 0) of
      Nothing -> (0, map read n, unwords c)
      Just d -> (d, map read (List.tail_err n), unwords c)

-- | Lines ending with @\@ continue to next line.
join_long_lines :: [String] -> [String]
join_long_lines l =
  case l of
    p : q : l' -> case List.separate_last' p of
      (p', Just '\\') -> join_long_lines ((p' ++ q) : l')
      _ -> p : join_long_lines (q : l')
    _ -> l

-- | Parse joined non-comment lines of modenam file.
parse_modenam :: [String] -> ModeNam
parse_modenam l =
  case l of
    n_str : x_str : m_str ->
      let n = read n_str :: Int
          x = read x_str :: Int
          m = map (parse_modenam_entry . words) m_str
      in if n == length m then (n, x, m) else error "parse_modenam"
    _ -> error "parse_modenam"

-- * Io

{- | 'parse_modenam' of 'Scala.load_dist_file' of @modenam.par@.

>>> mn <- load_modenam
>>> let (n,x,m) = mn
>>> (n, x, length m) -- Scala 2.64p
(3087,15,3087)

>>> m !! 0
(0,[1,5],"Vietnamese ditonic")
-}
load_modenam :: IO ModeNam
load_modenam = do
  l <- Scala.load_dist_file_ln "modenam.par"
  return (parse_modenam (Scala.filter_comments (join_long_lines l)))
