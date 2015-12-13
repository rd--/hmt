-- | @key: value@ database, allows duplicate @key@s.
module Music.Theory.DB.Plain where

import Data.List.Split {- split -}
import Data.Maybe {- base -}

import qualified Music.Theory.Function as T {- hmt -}
import qualified Music.Theory.IO as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

import Music.Theory.DB.Common {- hmt -}

entry_parse :: String -> Maybe Entry'
entry_parse s =
    case break (== ':') s of
      (k,':':' ':v) -> if null k || null v then Nothing else Just (k,v)
      _ -> Nothing

entry_pp :: Entry' -> String
entry_pp (k,v) = k ++ ": " ++ v

record_parse :: [String] -> Record'
record_parse = mapMaybe entry_parse

record_pp :: Record' -> String
record_pp = unlines . map entry_pp

is_entry :: String -> Bool
is_entry = isJust . entry_parse

db_parse :: String -> DB'
db_parse s =
    let l = filter (T.predicate_or is_entry null) (lines s)
        c = splitOn [""] l
    in map record_parse c

db_sort :: [(String,Int)] -> DB' -> DB'
db_sort k = T.sort_by_n_stage (map record_lookup_at k)

db_load_utf8 :: FilePath -> IO DB'
db_load_utf8 = fmap db_parse . T.read_file_utf8

db_store_utf8 :: FilePath -> DB' -> IO ()
db_store_utf8 fn = T.write_file_utf8 fn . unlines . map record_pp
