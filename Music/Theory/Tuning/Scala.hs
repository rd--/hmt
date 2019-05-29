{- | Parser for the Scala scale file format.

See <http://www.huygens-fokker.org/scala/scl_format.html> for details.

This module succesfully parses all scales 4809 in v.87 of the scale library.

-}
module Music.Theory.Tuning.Scala where

import Control.Monad {- base -}
import Data.Either {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import System.Directory {- directory -}
import System.Environment {- base -}
import System.FilePath {- filepath -}

import qualified Music.Theory.Directory as T {- hmt -}
import qualified Music.Theory.Either as T {- hmt -}
import qualified Music.Theory.Function as T {- hmt -}
import qualified Music.Theory.IO as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}
import qualified Music.Theory.Show as T {- hmt -}
import qualified Music.Theory.String as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}

-- * Pitch

-- | A @.scl@ pitch is either in 'Cents' or is a 'Ratio'.
type Pitch = Either T.Cents Rational

-- | An enumeration type for @.scl@ pitch classification.
data Pitch_Type = Pitch_Cents | Pitch_Ratio deriving (Eq,Show)

-- | A nearness value for deriving approximate rationals.
type Epsilon = Double

-- | Derive 'Pitch_Type' from 'Pitch'.
pitch_type :: Pitch -> Pitch_Type
pitch_type = either (const Pitch_Cents) (const Pitch_Ratio)

-- | Pitch as 'T.Cents', conversion by 'T.ratio_to_cents' if necessary.
pitch_cents :: Pitch -> T.Cents
pitch_cents p =
    case p of
      Left c -> c
      Right r -> T.ratio_to_cents r

-- | Pitch as 'Rational', conversion by 'T.reconstructed_ratio' if
-- necessary, hence /epsilon/.
pitch_ratio :: Epsilon -> Pitch -> Rational
pitch_ratio epsilon p =
    case p of
      Left c -> T.reconstructed_ratio epsilon c
      Right r -> r

-- | A pair giving the number of 'Cents' and number of 'Ratio' pitches.
pitch_representations :: [Pitch] -> (Int,Int)
pitch_representations =
    let f (l,r) p = case p of
                      Left _ -> (l + 1,r)
                      Right _ -> (l,r + 1)
    in foldl f (0,0)

-- | If scale is uniform, give type.
uniform_pitch_type :: [Pitch] -> Maybe Pitch_Type
uniform_pitch_type p =
    case pitch_representations p of
      (0,_) -> Just Pitch_Ratio
      (_,0) -> Just Pitch_Cents
      _ -> Nothing

-- | The predominant type of the pitches for 'Scale'.
pitch_type_predominant :: [Pitch] -> Pitch_Type
pitch_type_predominant p =
    let (c,r) = pitch_representations p
    in if c >= r then Pitch_Cents else Pitch_Ratio

-- * Scale

-- | A scale has a name, a description, a degree, and a list of 'Pitch'es.
type Scale = (String,String,Int,[Pitch])

-- | The name of a scale.
scale_name :: Scale -> String
scale_name (nm,_,_,_) = nm

-- | Text description of a scale.
scale_description :: Scale -> String
scale_description (_,d,_,_) = d

-- | The degree of the scale (number of 'Pitch'es).
scale_degree :: Scale -> Int
scale_degree (_,_,n,_) = n

-- | The 'Pitch'es at 'Scale'.
scale_pitches :: Scale -> [Pitch]
scale_pitches (_,_,_,p) = p

-- | Ensure degree and number of pitches align.
scale_verify :: Scale -> Bool
scale_verify (_,_,n,p) = n == length p

-- | Raise error if scale doesn't verify, else 'id'.
scale_verify_err :: Scale -> Scale
scale_verify_err scl = if scale_verify scl then scl else error "invalid scale"

-- | The last 'Pitch' element of the scale (ie. the /octave/).  For empty scales give 'Nothing'.
scale_octave :: Scale -> Maybe Pitch
scale_octave (_,_,_,s) =
    case s of
      [] -> Nothing
      _ -> Just (last s)

-- | Error variant.
scale_octave_err :: Scale -> Pitch
scale_octave_err = fromMaybe (error "scale_octave?") . scale_octave

-- | Is 'scale_octave' perfect, ie. 'Ratio' of @2@ or 'Cents' of @1200@.
perfect_octave :: Scale -> Bool
perfect_octave s =
  case scale_octave s of
    Just (Right 2) -> True
    Just (Left 1200) -> True
    _ -> False

-- | Are all pitches of the same type.
is_scale_uniform :: Scale -> Bool
is_scale_uniform = isJust . uniform_pitch_type . scale_pitches

-- | Make scale pitches uniform, conforming to the most predominant pitch type.
scale_uniform :: Epsilon -> Scale -> Scale
scale_uniform epsilon (nm,d,n,p) =
    case pitch_type_predominant p of
      Pitch_Cents -> (nm,d,n,map (Left . pitch_cents) p)
      Pitch_Ratio -> (nm,d,n,map (Right . pitch_ratio epsilon) p)

-- | Scale as list of 'T.Cents' (ie. 'pitch_cents') with @0@ prefix.
scale_cents :: Scale -> [T.Cents]
scale_cents s = 0 : map pitch_cents (scale_pitches s)

-- | 'map' 'round' of 'scale_cents'.
scale_cents_i :: Scale -> [T.Cents_I]
scale_cents_i = map round . scale_cents

-- | Scale as list of 'Rational' (ie. 'pitch_ratio') with @1@ prefix.
scale_ratios :: Epsilon -> Scale -> [Rational]
scale_ratios epsilon s = 1 : map (pitch_ratio epsilon) (scale_pitches s)

-- | Require that 'Scale' be uniformly of 'Ratio's.
scale_ratios_u :: Scale -> Maybe [Rational]
scale_ratios_u scl =
  let err = error "scale_ratios_u"
      p = scale_pitches scl
  in case uniform_pitch_type p of
       Just Pitch_Ratio -> Just (1 : map (fromMaybe err . T.from_right) p)
       _ -> Nothing

-- | Erroring variant of 'scale_ratios_u.
scale_ratios_req :: Scale -> [Rational]
scale_ratios_req = fromMaybe (error "scale_ratios_req") . scale_ratios_u

-- | Translate 'Scale' to 'T.Tuning'.  If 'Scale' is uniformly
-- rational, 'T.Tuning' is rational, else 'T.Tuning' is in 'T.Cents'.
-- 'Epsilon' is used to recover the 'Rational' octave if required.
scale_to_tuning :: Epsilon -> Scale -> T.Tuning
scale_to_tuning epsilon (_,_,_,p) =
    case partitionEithers p of
      ([],r) -> let (r',o) = T.separate_last r
                in T.Tuning (Left (1 : r')) o
      _ -> let (c,o) = T.separate_last p
               c' = 0 : map pitch_cents c
               o' = either (T.reconstructed_ratio epsilon) id o
           in T.Tuning (Right c') o'

-- | Convert 'T.Tuning' to 'Scale'.
--
-- > tuning_to_scale ("et12","12 tone equal temperament") (T.equal_temperament 12)
tuning_to_scale :: (String,String) -> T.Tuning -> Scale
tuning_to_scale (nm,dsc) (T.Tuning p o) =
    let n = either length length p
        p' = either (map Right . tail) (map Left . tail) p ++ [Right o]
    in (nm,dsc,n,p')

{- | Are scales equal ('==') at degree and tuning data.

> db <- scl_load_db
> let r = [2187/2048,9/8,32/27,81/64,4/3,729/512,3/2,6561/4096,27/16,16/9,243/128,2/1]
> let Just py = find (scale_eq ("","",length r,map Right r)) db
> scale_name py == "pyth_12"

> let c = map T.ratio_to_cents r
> let Just py' = find (scale_eqv 0.00001 ("","",length c,map Left c)) db
> scale_name py' == "pyth_12"

-}
scale_eq :: Scale -> Scale -> Bool
scale_eq (_,_,d0,p0) (_,_,d1,p1) = d0 == d1 && p0 == p1

-- | Are scales equal at degree and 'intersect' to at least /k/ places of tuning data.
scale_eq_n :: Int -> Scale -> Scale -> Bool
scale_eq_n k (_,_,d0,p0) (_,_,d1,p1) = d0 == d1 && length (intersect p0 p1) >= k

-- | Is `s1` a proper subset of `s2`.
scale_sub :: Scale -> Scale -> Bool
scale_sub (_,_,d0,p0) (_,_,d1,p1) = d0 < d1 && intersect p0 p1 == p0

-- | Are scales equal at degree and equivalent to within /epsilon/ at 'pitch_cents'.
scale_eqv :: Epsilon -> Scale -> Scale -> Bool
scale_eqv epsilon (_,_,d0,p0) (_,_,d1,p1) =
    let (~=) p q = abs (pitch_cents p - pitch_cents q) < epsilon
    in d0 == d1 && all id (zipWith (~=) p0 p1)

-- * Parser

-- | Comment lines begin with @!@.
is_comment :: String -> Bool
is_comment x =
    case x of
      '!':_ -> True
      _ -> False

-- | Remove to end of line @!@ comments.
--
-- > remove_eol_comments " 1 ! comment" == " 1 "
remove_eol_comments :: String -> String
remove_eol_comments = takeWhile (/= '!')

-- | Remove comments and null lines and trailing comments.
--
-- > filter_comments ["!a","b","","c","d!e"] == ["b","c","d"]
filter_comments :: [String] -> [String]
filter_comments =
    map remove_eol_comments .
    filter (not . T.predicate_any [is_comment,null])

-- | Pitches are either cents (with decimal point, possibly trailing) or ratios (with @/@).
--
-- > map parse_pitch ["700.0","350.","3/2","2"] == [Left 700,Left 350,Right (3/2),Right 2]
parse_pitch :: String -> Pitch
parse_pitch p =
    if '.' `elem` p
    then Left (T.read_fractional_allow_trailing_point_err p)
    else Right (T.read_ratio_with_div_err p)

-- | Pitch lines may contain commentary.
parse_pitch_ln :: String -> Pitch
parse_pitch_ln x =
    case words x of
      p:_ -> parse_pitch p
      _ -> error (show ("parse_pitch_ln",words x))

-- | Parse @.scl@ file.
parse_scl :: String -> String -> Scale
parse_scl nm s =
    case filter_comments (lines (T.filter_cr s)) of
      t:n:p -> let scl = (nm,T.delete_trailing_whitespace t,T.read_err n,map parse_pitch_ln p)
               in scale_verify_err scl
      _ -> error "parse"

-- * IO

-- | Read the environment variable @SCALA_SCL_DIR@, which is a
-- sequence of directories used to locate scala files on.
--
-- > setEnv "SCALA_DIST_DIR" "/home/rohan/data/scala/87/scl"
scl_get_dir :: IO [String]
scl_get_dir = fmap splitSearchPath (getEnv "SCALA_SCL_DIR")

-- | Lookup the @SCALA_SCL_DIR@ environment variable, which must exist, and derive the filepath.
-- It is an error if the name has a file extension.
--
-- > mapM scl_derive_filename ["young-lm_piano","et12"]
scl_derive_filename :: FilePath -> IO FilePath
scl_derive_filename nm = do
  dir <- scl_get_dir
  when (null dir) (error "scl_derive_filename: SCALA_SCL_DIR: nil")
  when (hasExtension nm) (error "scl_derive_filename: name has extension")
  T.path_scan_err dir (nm <.> "scl")

-- | If the name is an absolute file path and has a @.scl@ extension,
-- then return it, else run 'scl_derive_filename'.
--
-- > scl_resolve_name "young-lm_piano"
-- > scl_resolve_name "/home/rohan/data/scala/87/scl/young-lm_piano.scl"
-- > scl_resolve_name "/home/rohan/data/scala/87/scl/unknown-tuning.scl"
scl_resolve_name :: String -> IO FilePath
scl_resolve_name nm =
    let ex_f x = if x then return nm else error "scl_resolve_name: file does not exist"
    in if isAbsolute nm && takeExtension nm == ".scl"
       then doesFileExist nm >>= ex_f
       else scl_derive_filename nm

-- | Load @.scl@ file, runs 'resolve_scl'.
--
-- > s <- scl_load "xenakis_chrom"
-- > pitch_representations (scale_pitches s) == (6,1)
-- > scale_ratios 1e-3 s == [1,21/20,29/23,179/134,280/187,11/7,100/53,2]
scl_load :: String -> IO Scale
scl_load nm = do
  fn <- scl_resolve_name nm
  s <- T.read_file_iso_8859_1 fn
  return (parse_scl (takeBaseName nm) s)

-- | 'scale_to_tuning' of 'scl_load'.
scl_load_tuning :: Epsilon -> String -> IO T.Tuning
scl_load_tuning epsilon = fmap (scale_to_tuning epsilon) . scl_load

{- | Load all @.scl@ files at /dir/. -}
scl_load_dir :: FilePath -> IO [Scale]
scl_load_dir d = T.dir_subset [".scl"] d >>= mapM scl_load

-- | Load Scala data base at 'scl_get_dir'.
--
-- > db <- scl_load_db
-- > length db == 4809 {- scala/87/scl/ -}
-- > mapM_ (putStrLn . unlines . scale_stat) (filter (not . perfect_octave) db)
scl_load_db :: IO [Scale]
scl_load_db = do
  dir <- scl_get_dir
  r <- mapM scl_load_dir dir
  return (concat r)

-- * PP

-- | Simple plain-text display of scale data.
scale_stat :: Scale -> [String]
scale_stat s =
    let ty = uniform_pitch_type (scale_pitches s)
    in ["scale-name        : " ++ scale_name s
       ,"scale-description : " ++ scale_description s
       ,"scale-degree      : " ++ show (scale_degree s)
       ,"scale-type        : " ++ maybe "non-uniform" show ty
       ,"perfect-octave    : " ++ show (perfect_octave s)
       ,"scale-cents-i     : " ++ show (scale_cents_i s)
       ,if ty == Just Pitch_Ratio
        then "scale-ratios      : " ++ intercalate "," (map T.rational_pp (scale_ratios_req s))
        else ""]

-- | Pretty print 'Pitch' in @Scala@ format.
pitch_pp :: Pitch -> String
pitch_pp p =
    case p of
      Left c -> show c
      Right r -> show (numerator r) ++ "/" ++ show (denominator r)

-- | Pretty print 'Scale' in @Scala@ format.
--
-- > scl <- scl_load "et19"
-- > scl <- scl_load "young-lm_piano"
-- > putStr $ unlines $ scale_pp scl
scale_pp :: Scale -> [String]
scale_pp (nm,dsc,k,p) =
    ["! " ++ nm ++ ".scl"
    ,"!"
    ,dsc
    ,show k
    ,"!"] ++ map pitch_pp p

-- * DIST

-- | @scala@ distribution directory, given at @SCALA_DIST_DIR@.
--
-- > fmap (== "/home/rohan/opt/build/scala-22-pc64-linux") dist_get_dir
dist_get_dir :: IO String
dist_get_dir = getEnv "SCALA_DIST_DIR"

-- | Load file from 'dist_get_dir'.
--
-- > s <- load_dist_file "intnam.par"
-- > length s == 516
load_dist_file :: FilePath -> IO [String]
load_dist_file nm = do
  d <- dist_get_dir
  fmap lines (readFile (d </> nm))

-- * QUERY

{-
> db <- scl_load_db
> c = [0,83,199,308,388,507,579,695,778,899,1004,1084,1200]
> c = [-7,76,186,302,383,497,579,695,773,890,1004,1081,1200]
> r = scl_db_query_cdiff_asc db c
> mapM_ (putStrLn . unlines . scale_stat . snd) (take 20 r)
-}
scl_db_query_cdiff_asc :: [Scale] -> [T.Cents_I] -> [(Int,Scale)]
scl_db_query_cdiff_asc db c =
  let n = length c - 1
      db_f = filter ((== n) . scale_degree) db
      c_d = T.d_dx c
      c_r = map (T.dx_d 0) (T.rotations c_d)
      ndiff x = sum . map abs . zipWith (-) x
      ndiff_all_r scl = let x = scale_cents_i scl in minimum . map (ndiff x)
  in sort (map (\scl -> (ndiff_all_r scl c_r,scl)) db_f)
