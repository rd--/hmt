-- | JSON string association database.
-- JSON objects do no allow multiple keys.
-- Here multiple keys are read & written as arrays.
-- This is not built since it is little and introduces dependencies.
module Music.Theory.DB.JSON where

import Data.Bifunctor {- base -}
import Data.Maybe {- base -}

import qualified Data.Aeson.Micro as J {- microaeson -}
import qualified Data.ByteString.Lazy as B {- bytestring -}
import qualified Data.Map as Map {- containers -}
import qualified Data.Text as Text {- containers -}

import qualified Music.Theory.DB.Common as DB {- hmt -}

-- | Load 'DB' from 'FilePath'.
db_load_utf8 :: FilePath -> IO DB.DB'
db_load_utf8 fn = do
  let decode_assoc a =
        case a of
          J.Object o -> head (map (first Text.unpack) (Map.toList o))
          _ -> error "decode_assoc?"
      decode_record r =
        case r of
          J.Array l -> DB.record_uncollate (map (second (maybe_list_to_list . json_to_maybe_list_err) . decode_assoc) l)
          _ -> error "decode_record?"
  b <- B.readFile fn
  case J.decode b of
    Just (J.Array l) -> return (map decode_record l)
    _ -> return []

{- | Store 'DB' to 'FilePath'.

> import qualified Music.Theory.DB.Plain as P {- hmt -}
> let fn = "/home/rohan/ut/www-spr/data/db.text"
> db <- P.db_load_utf8 P.sep_plain fn
> length db == 1480
> db_store_utf8 "/tmp/sp.js" db
-}
db_store_utf8 :: FilePath -> DB.DB' -> IO ()
db_store_utf8 fn db = do
  let encode_assoc = J.Object . Map.fromList . return . first Text.pack
      f = J.Array .
          map (encode_assoc . second (maybe_list_to_json . list_to_maybe_list)) .
          DB.record_collate
      b = J.encode (J.Array (map f db))
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

-- | JSON to Maybe_List_Of_String.
--
-- > maybe_list_to_json (S "x") == J.String (Text.pack "x")
-- > maybe_list_to_json (L ["x","y"])
maybe_list_to_json :: Maybe_List_Of_String -> J.Value
maybe_list_to_json m =
  case m of
    S s -> J.String (Text.pack s)
    L l -> J.Array (map (J.String . Text.pack) l)

json_to_string_err :: J.Value -> String
json_to_string_err j =
  case j of
    J.String s -> Text.unpack s
    _ -> error "json_to_string?"

-- | JSON to Maybe_List_Of_String.
--
-- > :set -XOverloadedStrings
-- > json_to_maybe_list "\"x\"" == Just (S "x")
-- > json_to_maybe_list "[\"x\",\"y\"]" == Just (L ["x","y"])
json_to_maybe_list :: J.Value -> Maybe Maybe_List_Of_String
json_to_maybe_list j =
  case j of
    J.String s -> Just (S (Text.unpack s))
    J.Array l -> Just (L (map json_to_string_err l))
    _ -> Nothing

json_to_maybe_list_err :: J.Value -> Maybe_List_Of_String
json_to_maybe_list_err = fromMaybe (error "json_to_maybe_list") . json_to_maybe_list
