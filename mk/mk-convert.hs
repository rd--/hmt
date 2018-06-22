import Data.Char {- base -}
import Data.List {- base -}

-- > putStrLn $ gen_ts_conversion "Word8" "Int" "fromIntegral"
gen_ts_conversion :: String -> String -> String -> String
gen_ts_conversion p q f =
    let nm = map toLower p ++ "_to_" ++ map toLower q
    in concat ["-- | Type specialised '",f,"'\n",nm," :: ",p," -> ",q,"\n",nm," = ",f,"\n"]

gen_ts_conversion_set :: String -> [String] -> [String] -> [String]
gen_ts_conversion_set cnv p_ty q_ty =
  [gen_ts_conversion p q cnv | p <- p_ty, q <- q_ty, p /= q]

gen_type_abbvr :: (String, String) -> [String]
gen_type_abbvr (p,q) = ["--| Alias",concat ["type ",q," = ",p]]

-- > map gen_type_abbvr int_abbrev_table
int_abbrev_table :: [(String,String)]
int_abbrev_table =
  zip
  ["Word8","Word16","Word32","Word64","Int8","Int16","Int32","Int64","Int","Integer"]
  ["U8","U16","U32","U64","I8","I16","I32","I64","I","L"]

float_abbrev_table :: [(String,String)]
float_abbrev_table = zip ["Float","Double"] ["F32","F64"]

gen_int_conversions :: [String]
gen_int_conversions =
  let p = map fst int_abbrev_table
      q = map fst float_abbrev_table
  in gen_ts_conversion_set "fromIntegral" p (p ++ q)

-- > mapM_ putStrLn gen_int_conversions_abbrev
gen_int_conversions_abbrev :: [String]
gen_int_conversions_abbrev =
  let p = map snd int_abbrev_table
      q = map snd float_abbrev_table
  in gen_ts_conversion_set "fromIntegral" p (p ++ q)

-- > putStrLn $ gen_ts_conversion_maybe "Word8" "Int" "fromIntegral"
gen_ts_conversion_maybe :: String -> String -> String -> String
gen_ts_conversion_maybe p q f =
    let nm = map toLower p ++ "_to_" ++ map toLower q ++ "_maybe"
    in concat ["-- | Type specialised '",f,"'\n"
              ,nm," :: ",p," -> Maybe ",q,"\n"
              ,nm," n =\n"
              ,"    if n < fromIntegral (minBound::",q,") ||\n"
              ,"       n > fromIntegral (maxBound::",q,")\n"
              ,"    then Nothing\n"
              ,"    else Just (fromIntegral n)\n"]

gen_int_conversions_maybe :: [String]
gen_int_conversions_maybe =
    let p_ty = map fst int_abbrev_table
        q_ty = delete "Integer" p_ty
    in [gen_ts_conversion_maybe p q "fromIntegral" | p <- p_ty, q <- q_ty, p /= q]

main :: IO ()
main = do
  mapM_ putStrLn gen_int_conversions
  mapM_ putStrLn gen_int_conversions_maybe
