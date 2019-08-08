import System.Environment {- base -}

import qualified Music.Theory.DB.CSV as CSV
import qualified Music.Theory.DB.Common as Common
import qualified Music.Theory.DB.JSON as JSON
import qualified Music.Theory.DB.Plain as Plain

db_load_ty :: String -> FilePath -> IO (Common.DB String String)
db_load_ty ty fn =
    case ty of
      "plain" -> fmap (map Common.record_uncollate) (Plain.db_load_utf8 Plain.sep_plain fn)
      "json" -> JSON.db_load_utf8 fn
      "csv" -> CSV.db_load_utf8 fn
      _ -> error "db_load_ty"

db_store_ty :: String -> FilePath -> Common.DB String String -> IO ()
db_store_ty ty fn =
    case ty of
      "plain" -> Plain.db_store_utf8 Plain.sep_plain fn . map Common.record_collate
      "json" -> JSON.db_store_utf8 fn
      "csv" -> CSV.db_store_utf8 fn
      _ -> error "db_store_ty"

-- > convert ("plain","csv") ("/home/rohan/ut/www-spr/data/db.text","/tmp/t.csv")
-- > convert ("csv","json") ("/tmp/t.csv","/tmp/t.json")
convert :: (String,String) -> (FilePath,FilePath) -> IO ()
convert (input_ty,output_ty) (input_fn,output_fn) = do
  db <- db_load_ty input_ty input_fn
  db_store_ty output_ty output_fn db

-- > stat "plain" "/home/rohan/ut/inland/db/artists.text"
stat :: String -> FilePath -> IO ()
stat ty fn = do
  db <- db_load_ty ty fn
  let ks = Common.db_key_set db
  print ("#-records",length db)
  print ("#-keys",length ks)
  print ("key-set",unwords ks)

help :: [String]
help =
    ["convert input-type output-type input-file output-file"
    ,"stat type file-name"
    ,""
    ,"  type = csv | json | plain"]

main :: IO ()
main = do
  a <- getArgs
  case a of
    ["convert",i_ty,o_ty,i_fn,o_fn] -> convert (i_ty,o_ty) (i_fn,o_fn)
    ["stat",ty,fn] -> stat ty fn
    _ -> putStrLn (unlines help)
