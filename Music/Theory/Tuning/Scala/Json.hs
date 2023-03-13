-- | Scala Db Ji Json
module Music.Theory.Tuning.Scala.Json where

import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}

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

ji_tuning_json :: JiTuning -> String
ji_tuning_json (nm, dsc, iseq, oct, sq) =
  let q x = '"' : quote x ++ ['"']
      e x y = '\t' : q x ++ ": " ++ y
      i x = "[\n\t\t" ++ intercalate ",\n\t\t" (map show x) ++ "\n\t]"
  in concat
     [q nm
     ,": {\n"
     ,intercalate
       ",\n"
       [e "name" (q nm)
       ,e "description" (q dsc)
       ,e "degree" (show (length iseq))
       ,e "limit" (show (maximum (concatMap prime_factors iseq)))
       ,e "tuning" (i iseq)
       ]
     ,maybe "" (\x -> ",\n" ++ e "octave" (i [numerator x, denominator x])) oct
     ,maybe "" (\x -> ",\n" ++ e "sequence" (i x)) sq
     ,"\n}"]

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
  writeFile fn ("{\n" ++ intercalate ",\n" e ++ "\n}")

-- | Cents are written as numbers, ratios as [numerator, denominator] two-vectors.
pitch_json :: Pitch -> String
pitch_json p =
    case p of
      Left c -> show c
      Right r -> concat ["[", show (numerator r), ", ", show (denominator r), "]"]

-- | Format Scale as Json string.  Pitches are written as a string.
scale_json :: Scale -> String
scale_json (nm,dsc,k,p) =
  let q x = '"' : quote x ++ ['"']
      a x = "[" ++ intercalate ", " x ++ "]"
      e f x y = "\t" ++ q x ++ ": " ++ f y
  in unlines
     ["\t" ++ q nm ++ ": {"
     ,"\t" ++ intercalate
       ",\n\t"
       [e q "name" nm
       ,e q "description" dsc
       ,e id "degree" (show k)
       ,e a "pitches" (map pitch_json (drop_last p))
       ,e id "octave" (pitch_json (last p))]
     ,"\t}"]

{- | Write Scala database to Json.

> write_scala_db_json "/home/rohan/sw/hmt/data/json/scala-db.json"
-}
write_scala_db_json :: FilePath -> IO ()
write_scala_db_json fn = do
  db <- scl_load_db_dir
  writeFile fn ("{\n" ++ intercalate "\t,\n" (map scale_json db) ++ "}")

{-

db <- scl_load_db_dir -- v.91
length db == 5176

ji = map scale_ji_tuning (filter scl_is_ji db)
length ji == 2729
lm = filter ji_tuning_requires_large_integer ji
length lm == 63

-}
