-- | Parser for the Scala @intnam.par@ file.
module Music.Theory.Tuning.Scala.Interval where

import Data.Char {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Music.Theory.Read as Read {- hmt -}
import qualified Music.Theory.Tuning.Scala as Scala {- hmt -}

-- | Interval and name, ie. (3/2,"perfect fifth")
type Interval = (Rational,String)

-- | Length prefixed list of 'Interval'.
type IntNam = (Int,[Interval])

{- | Lookup ratio in 'IntNam'.

> db <- load_intnam
> intnam_search_ratio db (3/2) == Just (3/2,"perfect fifth")
> intnam_search_ratio db (2/3) == Nothing
> intnam_search_ratio db (4/3) == Just (4/3,"perfect fourth")
> intnam_search_ratio db (31/16) == Just (31/16,"=31st harmonic")
> intnam_search_ratio db (64/49) == Just (64 % 49,"=2 septatones or septatonic major third")
> map (intnam_search_ratio db) [3/2,4/3,7/4,7/6,9/7,9/8,12/7,14/9]
> import Data.Maybe {- base -}
> mapMaybe (intnam_search_ratio db) [567/512,147/128,21/16,1323/1024,189/128,49/32,441/256,63/32]
-}
intnam_search_ratio :: IntNam -> Rational -> Maybe Interval
intnam_search_ratio (_,i) x = find ((== x) . fst) i

{- | Lookup approximate ratio in 'IntNam' given espilon.

> r = [Just (3/2,"perfect fifth"),Just (64/49,"=2 septatones or septatonic major third")]
> map (intnam_search_fratio 0.0001 db) [1.5,1.3061] == r
-}
intnam_search_fratio :: (Fractional n,Ord n) => n -> IntNam -> n -> Maybe Interval
intnam_search_fratio epsilon (_,i) x =
  let near p q = abs (p - q) < epsilon
  in find (near x . fromRational . fst) i

-- | Lookup name of interval, or error.
intnam_search_ratio_name_err :: IntNam -> Rational -> String
intnam_search_ratio_name_err db = snd . fromJust . intnam_search_ratio db

-- | Lookup interval name in 'IntNam', ci = case-insensitive.
--
-- > db <- load_intnam
-- > intnam_search_description_ci db "didymus" == [(81/80,"syntonic comma, Didymus comma")]
intnam_search_description_ci :: IntNam -> String -> [Interval]
intnam_search_description_ci (_,i) x =
    let downcase = map toLower
        x' = downcase x
    in filter (isInfixOf x' . downcase . snd) i

-- * Parser

-- | Parse line from intnam.par
parse_intnam_entry :: String -> Interval
parse_intnam_entry str =
    case words str of
      r:w -> (Read.read_ratio_with_div_err False r,unwords w)
      _ -> error "parse_intnam_entry"

-- | Parse non-comment lines from intnam.par
parse_intnam :: [String] -> IntNam
parse_intnam l =
    case l of
      _:n:i -> let n' = read n :: Int
                   i' = map parse_intnam_entry i
               in if n' == length i' then (n',i') else error "parse_intnam"
      _ -> error "parse_intnam"

-- * Io

{- | 'parse_intnam' of 'Scala.load_dist_file_ln' of "intnam.par".

> intnam <- load_intnam
> fst intnam == 516 -- Scala 2.42p
> fst intnam == length (snd intnam)
> lookup (129140163/128000000) (snd intnam) == Just "gravity comma"
-}
load_intnam :: IO IntNam
load_intnam = do
  l <- Scala.load_dist_file_ln "intnam.par"
  return (parse_intnam (Scala.filter_comments l))
