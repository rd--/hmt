import Data.Char {- base -}
import Data.List {- base -}

-- > putStrLn $ gen_ts_conversion "Word8" "Int" "fromIntegral"
gen_ts_conversion :: String -> String -> String -> String
gen_ts_conversion p q f =
    let nm = map toLower p ++ "_to_" ++ map toLower q
    in concat ["-- | Type specialised '",f,"'\n",nm," :: ",p," -> ",q,"\n",nm," = ",f,"\n"]

-- > mapM_ putStrLn gen_int_conversions
gen_int_conversions :: [String]
gen_int_conversions =
    let p_ty = ["Word8","Word16","Word32","Int16","Int32","Int","Integer"]
        q_ty = p_ty ++ ["Float","Double"]
    in [gen_ts_conversion p q "fromIntegral" | p <- p_ty, q <- q_ty, p /= q]

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

-- > mapM_ putStrLn gen_int_conversions_maybe
gen_int_conversions_maybe :: [String]
gen_int_conversions_maybe =
    let p_ty = ["Word8","Word16","Word32","Int16","Int32","Int","Integer"]
        q_ty = delete "Integer" p_ty
    in [gen_ts_conversion_maybe p q "fromIntegral" | p <- p_ty, q <- q_ty, p /= q]
