module Music.Theory.DB.Char where

import Data.Char {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}
import Safe {- safe -}

import Music.Theory.Function {- hmt -}
import Music.Theory.List {- hmt -}

type Entry = (Char,String)
type Record = [Entry]

entry_parse :: String -> Maybe Entry
entry_parse s =
    case s of
      c:':':' ':s' -> if isUpper c then Just (c,s') else Nothing
      _ -> Nothing

entry_pp :: Entry -> String
entry_pp (c,s) = c : ':' : ' ' : s

record_pp :: Record -> String
record_pp = unlines . map entry_pp

record_lookup :: (Char,Int) -> Record -> Maybe String
record_lookup (c,n) = fmap snd . flip atMay n . filter ((== c) . fst)

is_entry :: String -> Bool
is_entry = isJust . entry_parse

db_parse :: String -> [Record]
db_parse s =
    let l = filter (is_entry `predicate_or` null) (lines s)
        c = splitOn [""] l
    in map (mapMaybe entry_parse) c

db_sort :: [Record] -> [Record]
db_sort = sort_by_two_stage (record_lookup ('A',0)) (record_lookup ('T',0))
