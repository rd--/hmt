import Data.Bifunctor {- base -}
import Data.Char {- base -}
import Data.Int {- base -}
import Data.List {- base -}

-- * COMMON

-- | Safe conversion, no bounds check.
--
-- > putStrLn $ gen_ts_conversion "Word8" "Int" "fromIntegral"
gen_ts_conversion :: String -> String -> String -> String
gen_ts_conversion p q f =
    let nm = map toLower p ++ "_to_" ++ map toLower q
    in concat ["-- | Type specialised '",f,"'\n",nm," :: ",p," -> ",q,"\n",nm," = ",f,"\n"]

-- * FX

-- | FIXED-LENGTH INTEGRAL TYPE = (SIGN,SIZE) (ie. not Int or Integer)
type FX = (Bool, Int)

fx_short :: FX -> String
fx_short (sgn,sz) = (if sgn then "I" else "U") ++ show sz

fx_long :: FX -> String
fx_long (sgn,sz) = (if sgn then "Int" else "Word") ++ show sz

fx_range :: FX -> (Int64,Int64)
fx_range (sgn,sz) = if sgn then (-(2 ^ (sz - 1)),(2 ^ (sz - 1)) - 1) else (0,(2 ^ sz) - 1)

-- > map fx_range_str [(False,4),(True,4)]
fx_range_str :: FX -> (String,String)
fx_range_str = bimap show show . fx_range

-- > fx_range_str_for_bc (False,4) (True,8)
fx_range_str_for_bc :: (Bool, b) -> FX -> (String, String)
fx_range_str_for_bc (sgn1,_) f2 =
  let f (lhs,rhs) = if not sgn1 then ("0",rhs) else (lhs,rhs)
  in f (fx_range_str f2)

-- > map (\x -> (fx_short x,fx_range x)) fx_univ
fx_univ :: [FX]
fx_univ = [(sgn,sz) | sgn <- [False,True], sz <- [4,7,8,12,14,16,24,32,64]]

fx_univ_cnv :: [(FX, FX)]
fx_univ_cnv = [(p,q) | p <- fx_univ, q <- fx_univ, p /= q]

-- | I cannot safely go to U (ie. requires check).
--   U can go to I if size is GT.
--   Like can go to like if size if GTE.
fx_is_subset :: FX -> FX -> Bool
fx_is_subset (sgn1,sz1) (sgn2,sz2) =
  case (sgn1,sgn2) of
    (True,False) -> False
    (False,True) -> sz2 > sz1
    _ -> sz2 >= sz1

fx_lhs_requires_check :: FX -> FX -> Bool
fx_lhs_requires_check (sgn,_) _ = sgn

fx_rhs_requires_check :: FX -> FX -> Bool
fx_rhs_requires_check f1 f2 =
  case (f1,f2) of
    ((True,sz1),(False,sz2)) -> sz2 < sz1
    _ -> True

-- > putStrLn $ gen_bc_conversion True ("0","127") "I32" "U7" "fromIntegral"
-- > putStrLn $ gen_bc_conversion True ("0","255") "Int32" "Word8" "fromIntegral"
fx_gen_bc_conversion :: Bool -> (String,String) -> String -> String -> String -> String
fx_gen_bc_conversion rhs (l,r) p q f =
    let nm = map toLower p ++ "_to_" ++ map toLower q -- ++ "_err"
    in concat ["-- | Type specialised '",f,"' with out-of-range error.\n",nm," :: ",p," -> ",q,"\n"
              ,nm," x ="
              ," if x < ",l,if rhs then " || x > " ++ r else ""
              ," then error \"",nm,": OUT-OF-RANGE\""
              ," else ",f," x\n"]

fx_gen_conversion :: (FX -> String) -> (FX,FX) -> String
fx_gen_conversion nm (f1,f2) =
  if fx_is_subset f1 f2
  then gen_ts_conversion
       (nm f1) (nm f2) "fromIntegral"
  else fx_gen_bc_conversion
       (fx_rhs_requires_check f1 f2)
       (fx_range_str_for_bc f1 f2)
       (nm f1) (nm f2) "fromIntegral"

fx_type_abbvr :: (String, String) -> [String]
fx_type_abbvr (p,q) = ["-- | Alias",concat ["type ",p," = ",q]]

fx_abbrev_table :: [(String,String)]
fx_abbrev_table =
  let sz = zip [4,7,8,12,14,16,24,32,64] [8,8,8,16,16,16,32,32,64]
      fx = [((sgn,sz1),(sgn,sz2)) | sgn <- [False,True], (sz1,sz2) <- sz]
  in map (bimap fx_short fx_long) fx

fx_main :: IO ()
fx_main = do
  putStrLn $ unlines $ intercalate [""] $ map fx_type_abbvr fx_abbrev_table
  putStrLn $ unlines $ map (fx_gen_conversion fx_short) fx_univ_cnv

-- * PLAIN

gen_type_abbvr :: (String, String) -> [String]
gen_type_abbvr (p,q) = ["--| Alias",concat ["type ",q," = ",p]]

-- > putStrLn $ unlines $ map (unlines . gen_type_abbvr) int_abbrev_table
int_abbrev_table :: [(String,String)]
int_abbrev_table =
  zip
  ["Word8","Word16","Word32","Word64","Int8","Int16","Int32","Int64","Int"]
  ["U8","U16","U32","U64","I8","I16","I32","I64","Int"]

float_abbrev_table :: [(String,String)]
float_abbrev_table = zip ["Float","Double"] ["F32","F64"]

gen_ts_conversion_set :: String -> [String] -> [String] -> [String]
gen_ts_conversion_set cnv p_ty q_ty =
  [gen_ts_conversion p q cnv | p <- p_ty, q <- q_ty, p /= q]

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
