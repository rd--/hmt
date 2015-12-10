-- | Single character line oriented database format.
module Music.Theory.DB.Char where

import Data.Char {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}
import Safe {- safe -}

import qualified Music.Theory.Function as T {- hmt -}
import qualified Music.Theory.IO as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

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
    let l = filter (T.predicate_or is_entry null) (lines s)
        c = splitOn [""] l
    in map (mapMaybe entry_parse) c

db_sort :: [Record] -> [Record]
db_sort = T.sort_by_two_stage (record_lookup ('A',0)) (record_lookup ('T',0))

db_load_utf8 :: FilePath -> IO [Record]
db_load_utf8 = fmap db_parse . T.read_file_utf8
