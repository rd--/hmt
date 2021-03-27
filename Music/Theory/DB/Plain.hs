-- | @key: value@ database, allows duplicate @key@s.
module Music.Theory.DB.Plain where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.List.Split as Split {- split -}
import qualified Safe {- safe -}

import qualified Music.Theory.IO as IO {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

-- | (RECORD-SEPARATOR,FIELD-SEPARATOR,ENTRY-SEPARATOR)
type SEP = (String,String,String)

type Key = String
type Value = String
type Entry = (Key,[Value])
type Record = [Entry]
type DB = [Record]

sep_plain :: SEP
sep_plain = (['\n','\n'],['\n'],": ")

-- > record_parse (";","=") "F=f/rec;E=au;C=A;K=P;K=Q"
record_parse :: (String,String) -> String -> Record
record_parse (fs,es) = T.collate_adjacent . mapMaybe (T.separate_at es) . Split.splitOn fs

record_lookup :: Key -> Record -> [Value]
record_lookup k = fromMaybe [] . lookup k

record_lookup_at :: (Key,Int) -> Record -> Maybe Value
record_lookup_at (k,n) = flip Safe.atMay n . record_lookup k

record_has_key :: Key -> Record -> Bool
record_has_key k = isJust . lookup k

record_lookup_uniq :: Key -> Record -> Maybe Value
record_lookup_uniq k r =
    case record_lookup k r of
      [] -> Nothing
      [v] -> Just v
      _ -> error "record_lookup_uniq: non uniq"

db_parse :: SEP -> String -> [Record]
db_parse (rs,fs,es) s =
    let r = Split.splitOn rs s
    in map (record_parse (fs,es)) r

db_sort :: [(Key,Int)] -> [Record] -> [Record]
db_sort k = T.sort_by_n_stage_on (map record_lookup_at k)

db_load_utf8 :: SEP -> FilePath -> IO [Record]
db_load_utf8 sep = fmap (db_parse sep) . IO.read_file_utf8

-- > record_pp (";","=") [("F","f/rec.au"),("C","A")]
record_pp :: (String,String) -> Record -> String
record_pp (fs,es) = intercalate fs . map (\(k,v) -> k ++ es ++ v) . T.uncollate

db_store_utf8 :: SEP -> FilePath -> [Record] -> IO ()
db_store_utf8 (rs,fs,es) fn = IO.write_file_utf8 fn . intercalate rs . map (record_pp (fs,es))
