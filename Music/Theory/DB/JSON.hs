-- | JSON string association database.
-- JSON objects do no allow multiple keys.
-- Here multiple keys are read & written as arrays.
module Music.Theory.DB.JSON where

import qualified Data.Aeson as A {- aeson -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.Map as M {- containers -}

import qualified Music.Theory.DB.Common as DB

-- | Load 'DB' from 'FilePath'.
db_load_utf8 :: FilePath -> IO DB.DB'
db_load_utf8 fn = do
  b <- B.readFile fn
  case A.decode b of
    Just m ->
        let f = DB.record_uncollate .
                map (fmap maybe_list_to_list) .
                M.toList
        in return (map f m)
    Nothing -> return []

-- | Store 'DB' to 'FilePath'.
--
-- > let fn = "/home/rohan/ut/www-spr/data/db.js"
-- > db <- db_load_utf8 fn
-- > length db == 1334
-- > db_store_utf8 "/tmp/sp.js" db
db_store_utf8 :: FilePath -> DB.DB' -> IO ()
db_store_utf8 fn db = do
  let db' = let f = map (fmap list_to_maybe_list) . DB.record_collate
            in map f db
      b = A.encode (map M.fromList db')
  B.writeFile fn b

-- * Maybe List of String

data Maybe_List_Of_String = S String | L [String] deriving (Eq,Show)

maybe_list_to_list :: Maybe_List_Of_String -> [String]
maybe_list_to_list m =
    case m of
      S s -> [s]
      L l -> l

list_to_maybe_list :: [String] -> Maybe_List_Of_String
list_to_maybe_list l =
    case l of
      [s] -> S s
      _ -> L l

-- > A.toJSON (S "x")
-- > A.toJSON (L ["x","y"])
instance A.ToJSON Maybe_List_Of_String where
    toJSON (S s) = A.toJSON s
    toJSON (L l) = A.toJSON l

-- > :set -XOverloadedStrings
-- > A.decode "\"x\"" :: Maybe Maybe_List_Of_String
-- > A.decode "[\"x\",\"y\"]" :: Maybe Maybe_List_Of_String
instance A.FromJSON Maybe_List_Of_String where
    parseJSON v =
        case v of
          A.String _ -> fmap S (A.parseJSON v)
          A.Array _ -> fmap L (A.parseJSON v)
          _ -> error "parseJSON: Maybe_List_String"
