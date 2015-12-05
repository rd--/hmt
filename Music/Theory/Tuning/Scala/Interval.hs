-- | Parser for the @intnam.par@ file.
module Music.Theory.Tuning.Scala.Interval where

import Data.List {- base -}

import qualified Music.Theory.Read as T {- hmt -}
import qualified Music.Theory.Tuning.Scala as T

type INTERVAL = (Rational,String)

type INTNAM = (Int,[INTERVAL])

-- > intnam_search_ratio intnam (3/2) == Just (3/2,"perfect fifth")
-- > intnam_search_ratio intnam (2/3) == Nothing
-- > intnam_search_ratio intnam (4/3) == Just (4/3,"perfect fourth")
intnam_search_ratio :: INTNAM -> Rational -> Maybe INTERVAL
intnam_search_ratio (_,i) x = find ((== x) . fst) i

-- > intnam_search_description intnam "perfect"
intnam_search_description :: INTNAM -> String -> [INTERVAL]
intnam_search_description (_,i) x = filter (isInfixOf x . snd) i

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

-- > intnam <- load_intnam
load_intnam :: IO INTNAM
load_intnam = do
  l <- T.load_dist_file "intnam.par"
  return (parse_intnam (T.filter_comments l))
