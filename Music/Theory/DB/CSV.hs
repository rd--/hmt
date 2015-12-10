-- | Keys are given in the header, empty fields are omitted from records.
module Music.Theory.DB.CSV where

import Data.Maybe {- base -}
import qualified Text.CSV.Lazy.String as C {- lazy-csv -}

import qualified Music.Theory.DB.Common as DB {- hmt -}
import qualified Music.Theory.IO as T {- hmt -}

type Key = String
type Value = String
type Entry = (Key,Value)
type Record = [Entry]
type DB = [Record]

-- | Load 'DB' from 'FilePath'.
db_load_utf8 :: FilePath -> IO DB
db_load_utf8 fn = do
  s <- T.read_file_utf8 fn
  let p = C.fromCSVTable (C.csvTable (C.parseCSV s))
      (h,d) = (head p,tail p)
      f k v = if null v then Nothing else Just (k,v)
  return (map (catMaybes . zipWith f h) d)

db_store_utf8 :: FilePath -> DB -> IO ()
db_store_utf8 fn db = do
  let (hdr,tbl) = DB.db_to_table (fromMaybe "") db
      (_,tbl') = C.toCSVTable (hdr : tbl)
      str = C.ppCSVTable tbl'
  T.write_file_utf8 fn str
