-- | Parser for the @modename.par@ file.
module Music.Theory.Tuning.Scala.Mode where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.Environment {- base -}
import System.FilePath {- filepath -}

import qualified Music.Theory.Function as T
import qualified Music.Theory.List as T
import qualified Music.Theory.Tuning.Scala as T

type MODE = (Int,[Int],String)

mode_intervals :: MODE -> [Int]
mode_intervals (_,i,_) = i

mode_description :: MODE -> String
mode_description (_,_,d) = d

mode_degree :: MODE -> Int
mode_degree = sum . mode_intervals

type MODENAM = (Int,Int,[MODE])

modenam_modes :: MODENAM -> [MODE]
modenam_modes (_,_,m) = m

-- > let sq = modenam_search_seq mn
-- > sq [2,2,1,2,2,2,1]
-- > sq [2,1,2,2,1,2,2]
-- > sq [2,1,2,2,1,3,1]
-- > sq (replicate 6 2)
-- > sq [1,2,1,2,1,2,1,2]
-- > sq [2,1,2,1,2,1,2,1]
-- > sq (replicate 12 1)
modenam_search_seq :: MODENAM -> [Int] -> [MODE]
modenam_search_seq (_,_,m) x = filter ((== x) . mode_intervals) m

-- > modenam_search_description mn "Messiaen"
-- > modenam_search_description mn "Xenakis"
modenam_search_description :: MODENAM -> String -> [MODE]
modenam_search_description (_,_,m) x = filter (isInfixOf x . mode_description) m

-- * Parser

-- | Bracketed integers are a non-implicit starting degree.
--
-- > map non_implicit_degree ["4","[4]"] == [Nothing,Just 4]
non_implicit_degree :: String -> Maybe Int
non_implicit_degree s =
    case T.unbracket s of
      Just ('[',s',']') -> Just (read s')
      _ -> Nothing

is_non_implicit_degree :: String -> Bool
is_non_implicit_degree = isJust . non_implicit_degree

is_integer :: String -> Bool
is_integer = all isDigit

parse_modenam_entry :: [String] -> MODE
parse_modenam_entry w =
    let (n0:n,c) = span (T.predicate_or is_non_implicit_degree is_integer) w
    in case non_implicit_degree n0 of
         Nothing -> (0,map read (n0:n),unwords c)
         Just d -> (d,map read n,unwords c)

-- | Lines ending with @\@ continue to next line.
join_long_lines :: [String] -> [String]
join_long_lines l =
    case l of
      p:q:l' -> case T.separate_last' p of
                  (p',Just '\\') -> join_long_lines ((p' ++ q) : l')
                  _ -> p : join_long_lines (q : l')
      _ -> l

parse_modenam :: [String] -> MODENAM
parse_modenam l =
    case l of
      n:x:m -> let n' = read n :: Int
                   x' = read x :: Int
                   m' = map (parse_modenam_entry . words) m
               in if n' == length m' then (n',x',m') else error "parse_modenam"
      _ -> error "parse_modenam"

-- * IO

-- > setEnv "SCALA_DIST_DIR" "/home/rohan/opt/build/scala-22-pc64-linux"
-- > mn <- load_modenam
-- > let (n,x,m) = mn
load_modenam :: IO MODENAM
load_modenam = do
  d <- getEnv "SCALA_DIST_DIR"
  s <- readFile (d </> "modenam.par")
  let l = T.filter_comments (join_long_lines (lines s))
  return (parse_modenam l)
