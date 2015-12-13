-- | @key: value@ database, allows duplicate @key@s.
module Music.Theory.DB.Plain where

import Data.List {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}

import qualified Music.Theory.IO as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

import Music.Theory.DB.Common {- hmt -}

--is_entry :: String -> Bool
--is_entry = isJust . T.seperate_at ": "

-- | (RECORD-SEPARATOR,FIELD-SEPARATOR,ENTRY-SEPARATOR)
type SEP = (String,String,String)

sep_plain :: SEP
sep_plain = (['\n','\n'],['\n'],": ")

-- > record_parse (";","=") "F=f/rec.au;C=A"
record_parse :: (String, String) -> String -> Record'
record_parse (fs,es) = mapMaybe (T.seperate_at es) . splitOn fs

db_parse :: SEP -> String -> DB'
db_parse (rs,fs,es) s =
    let r = splitOn rs s
    in map (record_parse (fs,es)) r

db_sort :: [(String,Int)] -> DB' -> DB'
db_sort k = T.sort_by_n_stage (map record_lookup_at k)

db_load_utf8 :: SEP -> FilePath -> IO DB'
db_load_utf8 sep = fmap (db_parse sep) . T.read_file_utf8

-- > record_pp (";","=") [("F","f/rec.au"),("C","A")]
record_pp :: (String,String) -> Record' -> String
record_pp (fs,es) = intercalate fs . map (\(k,v) -> k ++ es ++ v)

db_store_utf8 :: SEP -> FilePath -> DB' -> IO ()
db_store_utf8 (rs,fs,es) fn = T.write_file_utf8 fn . intercalate rs . map (record_pp (fs,es))
