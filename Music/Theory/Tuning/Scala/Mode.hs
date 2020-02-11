-- | Parser for the @modename.par@ file.
module Music.Theory.Tuning.Scala.Mode where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Function as Function {- hmt -}
import qualified Music.Theory.List as List {- hmt -}
import qualified Music.Theory.Tuning.Scala as Scala {- hmt -}

-- | (start-degree,intervals,description)
type MODE = (Int,[Int],String)

mode_starting_degree :: MODE -> Int
mode_starting_degree (d,_,_) = d

mode_intervals :: MODE -> [Int]
mode_intervals (_,i,_) = i

mode_description :: MODE -> String
mode_description (_,_,d) = d

mode_degree :: MODE -> Int
mode_degree = sum . mode_intervals

-- | (mode-count,_,mode-list)
type MODENAM = (Int,Int,[MODE])

modenam_modes :: MODENAM -> [MODE]
modenam_modes (_,_,m) = m

-- | Search for mode by interval list.
modenam_search_seq :: MODENAM -> [Int] -> [MODE]
modenam_search_seq (_,_,m) x = filter ((== x) . mode_intervals) m

-- | Expect /one/ result.
--
-- > mn <- load_modenam
-- > let sq = putStrLn . unlines . mode_stat . fromJust . modenam_search_seq1 mn
-- > sq [2,2,1,2,2,2,1]
-- > sq [2,1,2,2,1,2,2]
-- > sq [2,1,2,2,1,3,1]
-- > sq (replicate 6 2)
-- > sq [1,2,1,2,1,2,1,2]
-- > sq [2,1,2,1,2,1,2,1]
-- > sq (replicate 12 1)
modenam_search_seq1 :: MODENAM -> [Int] -> Maybe MODE
modenam_search_seq1 mn = List.unlist1 . modenam_search_seq mn

-- | Search for mode by description text.
--
-- > map (modenam_search_description mn) ["Messiaen","Xenakis","Raga"]
modenam_search_description :: MODENAM -> String -> [MODE]
modenam_search_description (_,_,m) x = filter (isInfixOf x . mode_description) m

-- | Pretty printer.
mode_stat :: MODE -> [String]
mode_stat (d,i,s) =
    ["mode-start-degree : " ++ show d
    ,"mode-intervals    : " ++ intercalate "," (map show i)
    ,"mode-degree       : " ++ show (sum i)
    ,"mode-description  : " ++ s]

-- * Parser

-- | Bracketed integers are a non-implicit starting degree.
--
-- > map non_implicit_degree ["4","[4]"] == [Nothing,Just 4]
non_implicit_degree :: String -> Maybe Int
non_implicit_degree s =
    case List.unbracket s of
      Just ('[',x,']') -> Just (read x)
      _ -> Nothing

-- | Predicate form
is_non_implicit_degree :: String -> Bool
is_non_implicit_degree = isJust . non_implicit_degree

is_integer :: String -> Bool
is_integer = all isDigit

parse_modenam_entry :: [String] -> MODE
parse_modenam_entry w =
    let (n0:n,c) = span (Function.predicate_or is_non_implicit_degree is_integer) w
    in case non_implicit_degree n0 of
         Nothing -> (0,map read (n0:n),unwords c)
         Just d -> (d,map read n,unwords c)

-- | Lines ending with @\@ continue to next line.
join_long_lines :: [String] -> [String]
join_long_lines l =
    case l of
      p:q:l' -> case List.separate_last' p of
                  (p',Just '\\') -> join_long_lines ((p' ++ q) : l')
                  _ -> p : join_long_lines (q : l')
      _ -> l

-- | Parse joined non-comment lines of modenam file.
parse_modenam :: [String] -> MODENAM
parse_modenam l =
    case l of
      n_str:x_str:m_str ->
        let n = read n_str :: Int
            x = read x_str :: Int
            m = map (parse_modenam_entry . words) m_str
        in if n == length m then (n,x,m) else error "parse_modenam"
      _ -> error "parse_modenam"

-- * IO

-- | 'parse_modenam' of 'Scala.load_dist_file' of @modenam.par@.
--
-- > mn <- load_modenam
-- > let (n,x,m) = mn
-- > n == 2776 && x == 15 && length m == n
load_modenam :: IO MODENAM
load_modenam = do
  l <- Scala.load_dist_file_ln "modenam.par"
  return (parse_modenam (Scala.filter_comments (join_long_lines l)))
