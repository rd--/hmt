-- | @key: value@ database, allows duplicate @key@s.
module Music.Theory.DB.Plain where

import Data.List.Split {- split -}
import Data.Maybe {- base -}

import qualified Music.Theory.IO as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

import Music.Theory.DB.Common {- hmt -}

-- > seperate_at "//" "lhs//rhs" == Just ("lhs","rhs")
seperate_at :: Eq a => [a] -> [a] -> Maybe ([a],[a])
seperate_at x =
    let n = length x
        f lhs rhs =
            if null rhs
            then Nothing
                 else if x == take n rhs
                      then Just (reverse lhs,drop n rhs)
                      else f (head rhs : lhs) (tail rhs)
    in f []

entry_pp :: Entry' -> String
entry_pp (k,v) = k ++ ": " ++ v

record_pp :: Record' -> String
record_pp = unlines . map entry_pp

is_entry :: String -> Bool
is_entry = isJust . seperate_at ": "

-- | (RECORD-SEPARATOR,FIELD-SEPARATOR,ENTRY-SEPARATOR)
type SEP = (String,String,String)

sep_plain :: SEP
sep_plain = (['\n','\n'],['\n'],": ")

-- > record_parse (";","=") "F=f/rec.au;C=A"
record_parse :: (String, String) -> String -> Record'
record_parse (fs,es) = mapMaybe (seperate_at es) . splitOn fs

db_parse :: SEP -> String -> DB'
db_parse (rs,fs,es) s =
    let r = splitOn rs s
    in map (record_parse (fs,es)) r

db_sort :: [(String,Int)] -> DB' -> DB'
db_sort k = T.sort_by_n_stage (map record_lookup_at k)

db_load_utf8 :: SEP -> FilePath -> IO DB'
db_load_utf8 sep = fmap (db_parse sep) . T.read_file_utf8

db_store_utf8 :: FilePath -> DB' -> IO ()
db_store_utf8 fn = T.write_file_utf8 fn . unlines . map record_pp
