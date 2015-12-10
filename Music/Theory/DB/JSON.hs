-- | JSON string association database.
module Music.Theory.DB.JSON where

import qualified Data.Aeson as A {- aeson -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.Map as M {- containers -}

type Key = String
type Value = String
type Entry = (Key,Value)
type Record = [Entry]
type DB = [Record]

-- | Load 'DB' from 'FilePath'.
db_load_utf8 :: FilePath -> IO DB
db_load_utf8 fn = do
  b <- B.readFile fn
  case A.decode b of
    Just m -> return (map M.toList m)
    Nothing -> return []

-- | Store 'DB' to 'FilePath'.
--
-- > let fn = "/home/rohan/ut/www-spr/data/db.js"
-- > db <- db_load_utf8 fn
-- > length db == 1334
-- > db_store_utf8 "/tmp/sp.js" db
db_store_utf8 :: FilePath -> DB -> IO ()
db_store_utf8 fn db = do
  let b = A.encode (map M.fromList db)
  B.writeFile fn b
