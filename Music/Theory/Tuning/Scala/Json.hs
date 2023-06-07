-- | Scala Db Ji Json
module Music.Theory.Tuning.Scala.Json where

import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}

import qualified Music.Theory.Json as Json {- hmt-base -}
import Music.Theory.List {- hmt-base -}
import Music.Theory.Math.Prime {- hmt-base -}

import Music.Theory.Tuning.Scala {- hmt -}

type Name = String
type Description = String
type JiTuning = (Name, Description, [Integer], Maybe Rational, Maybe [Int])

float64_integer_max :: Integer
float64_integer_max = (2 ^ (53::Integer)) - 1

ji_tuning_name :: JiTuning -> Name
ji_tuning_name (x, _, _, _, _) = x

ji_tuning_requires_large_integer :: JiTuning -> Bool
ji_tuning_requires_large_integer (_, _, i, _, _) = any (\x -> x >= float64_integer_max) i

ji_tuning_degree :: JiTuning -> Int
ji_tuning_degree (_, _, x, _, _) = length x

ji_tuning_limit :: JiTuning -> Integer
ji_tuning_limit (_, _, x, _, _) = maximum (concatMap prime_factors x)

rational_to_integer :: Rational -> Integer
rational_to_integer r =
  if denominator r == 1
  then numerator r
  else error "rational_to_integer?"

scale_ji_tuning :: Scale -> JiTuning
scale_ji_tuning scl =
  let (r, o) = separate_last (scale_ratios_req True scl)
      d = map denominator r
      m = foldr1 lcm d % 1
      s = map (rational_to_integer . (* m)) r
      i = nub (sort s)
      i' = if i == s
           then Nothing
           else Just (map (\x -> fromMaybe (error "?") (elemIndex x i) + 1) s)
      o' = if o == 2
           then Nothing
           else Just o
  in (scale_name scl, scale_description scl, i, o', i')

quote :: String -> String
quote =
  let f x = if x `elem` ['"', '\\'] then ['\\', x] else [x]
  in concatMap f

ji_tuning_json :: JiTuning -> Json.Association
ji_tuning_json (nm, dsc, iseq, oct, sq) =
  let integerArray = Json.array . map Json.integer
      intArray = Json.array . map Json.int
  in (nm
      ,Json.object ([
          ("name", Json.string nm)
          ,("description", Json.string dsc)
          ,("degree", Json.int (length iseq))
          ,("limit", Json.integer (maximum (concatMap prime_factors iseq)))
          ,("tuning", integerArray iseq)] ++ catMaybes [
                       fmap (\x -> ("octave", integerArray [numerator x, denominator x])) oct
                       ,fmap (\x -> ("sequence", intArray x)) sq]))

{- | Write Ji subset of Scala database to Json.
Pitches are stored as sorted sequences of integers.
Ratios are recovered by dividing each integer by the first (lowest).
Scales where the octave is not 2:1 store it as an [numerator, denominator] two-vector.

> write_ji_tuning_db "/home/rohan/sw/hmt/data/json/scala-ji-tuning.json"
-}
write_ji_tuning_db :: FilePath -> IO ()
write_ji_tuning_db fn = do
  db <- scl_load_db_dir
  let ji = map scale_ji_tuning (filter scl_is_ji db)
      lm = filter (not . ji_tuning_requires_large_integer) ji
      e = map ji_tuning_json lm
  Json.writeFile fn (Json.object e)

pitch_can_be_json :: Pitch -> Bool
pitch_can_be_json p =
    case p of
      Left _ -> True
      Right r -> all Json.isSafeIntegral [numerator r, denominator r]

-- | Cents are written as numbers, ratios as [numerator, denominator] two-vectors.
pitch_json :: Pitch -> Json.Value
pitch_json p =
    case p of
      Left c -> Json.double c
      Right r -> Json.array (map Json.integer [numerator r, denominator r])

scale_can_be_json :: Scale -> Bool
scale_can_be_json (_, _, _, p) = all pitch_can_be_json p

-- | Format Scale as Json string.  Pitches are written as a string.
scale_json :: Scale -> Json.Association
scale_json (nm,dsc,k,p) =
  (nm
  ,Json.object [
      ("name", Json.string nm)
      ,("description", Json.string dsc)
      ,("degree", Json.int k)
      ,("pitches", Json.array (map pitch_json (drop_last p)))
      ,("octave", pitch_json (last p))])

{- | Write Scala database to Json.

> write_scala_db_json "/home/rohan/sw/hmt/data/json/scala-db.json"
-}
write_scala_db_json :: FilePath -> IO ()
write_scala_db_json fn = do
  db <- scl_load_db_dir
  Json.writeFile fn (Json.object (map scale_json (filter scale_can_be_json db)))

{-

db <- scl_load_db_dir -- v.91
length db == 5176

filter (not . scale_can_be_json) db

ji = map scale_ji_tuning (filter scl_is_ji db)
length ji == 2729
lm = filter ji_tuning_requires_large_integer ji
length lm == 63

-}
