-- | Parser for the @intnam.par@ file.
module Music.Theory.Tuning.Scala.Interval where

import Data.Char {- base -}
import Data.List {- base -}

import qualified Music.Theory.Read as T {- hmt -}
import qualified Music.Theory.Tuning.Scala as T

type INTERVAL = (Rational,String)

type INTNAM = (Int,[INTERVAL])

-- > intnam <- load_intnam
-- > intnam_search_ratio intnam (3/2) == Just (3/2,"perfect fifth")
-- > intnam_search_ratio intnam (2/3) == Nothing
-- > intnam_search_ratio intnam (4/3) == Just (4/3,"perfect fourth")
-- > map (intnam_search_ratio intnam) [3/2,4/3,7/4,7/6,9/7,12/7,14/9]
-- > intnam_search_ratio intnam (31/16) == Just (31/16,"31st harmonic")
intnam_search_ratio :: INTNAM -> Rational -> Maybe INTERVAL
intnam_search_ratio (_,i) x = find ((== x) . fst) i

downcase :: String -> String
downcase = map toLower

-- > intnam <- load_intnam
-- > intnam_search_description intnam "didymus"
intnam_search_description_ci :: INTNAM -> String -> [INTERVAL]
intnam_search_description_ci (_,i) x =
    let x' = downcase x
    in filter (isInfixOf x' . downcase . snd) i

-- * Parser

parse_intnam_entry :: [String] -> INTERVAL
parse_intnam_entry w =
    case w of
      r:w' -> (T.read_ratio_with_div_err r,unwords w')
      _ -> error "parse_intnam_entry"

parse_intnam :: [String] -> INTNAM
parse_intnam l =
    case l of
      _:n:i -> let n' = read n :: Int
                   i' = map (parse_intnam_entry . words) i
               in if n' == length i' then (n',i') else error "parse_intnam"
      _ -> error "parse_intnam"

-- * IO

load_intnam :: IO INTNAM
load_intnam = do
  l <- T.load_dist_file "intnam.par"
  return (parse_intnam (T.filter_comments l))
