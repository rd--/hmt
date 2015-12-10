-- | Single character line oriented database format.
module Music.Theory.DB.Char where

import Data.Char {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}

import qualified Music.Theory.Function as T {- hmt -}
import qualified Music.Theory.IO as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

import qualified Music.Theory.DB.Common as DB {- hmt -}

type Key = Char
type Value = String
type Entry = (Key,Value)
type Record = [Entry]
type DB = [Record]

entry_parse :: String -> Maybe Entry
entry_parse s =
    case s of
      c:':':' ':s' -> if isUpper c then Just (c,s') else Nothing
      _ -> Nothing

entry_pp :: Entry -> String
entry_pp (c,s) = c : ':' : ' ' : s

record_pp :: Record -> String
record_pp = unlines . map entry_pp

is_entry :: String -> Bool
is_entry = isJust . entry_parse

db_parse :: String -> DB
db_parse s =
    let l = filter (T.predicate_or is_entry null) (lines s)
        c = splitOn [""] l
    in map (mapMaybe entry_parse) c

db_sort :: [(Key,Int)] -> DB -> DB
db_sort k = T.sort_by_n_stage (map DB.record_lookup_at k)

db_load_utf8 :: FilePath -> IO DB
db_load_utf8 = fmap db_parse . T.read_file_utf8
