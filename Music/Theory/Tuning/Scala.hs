-- | Parser for the Scala scale file format.  See
-- <http://www.huygens-fokker.org/scala/scl_format.html> for details.
-- This module succesfully parses all 4557 scales in v.83 of the scale
-- library.
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
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Math as T {- hmt -}
import qualified Music.Theory.Read as T {- hmt -}
import qualified Music.Theory.String as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}

-- * Types

-- | A @.scl@ pitch is either in 'Cents' or is a 'Ratio'.
data Pitch_Type = Pitch_Cents | Pitch_Ratio deriving (Eq,Show)

-- | A @.scl@ pitch is either in 'Cents' or is a 'Ratio'.
type Pitch i = Either T.Cents (Ratio i)

-- | A scale has a name, a description, a degree, and a list of 'Pitch'es.
type Scale i = (String,String,Int,[Pitch i])

-- | Ensure degree and number of pitches align.
scale_verify :: Scale i -> Bool
scale_verify (_,_,n,p) = n == length p

-- | Raise error if scale doesn't verify, else 'id'.
scale_verify_err :: Scale i -> Scale i
scale_verify_err scl = if scale_verify scl then scl else error "invalid scale"

-- | A nearness value for deriving approximate rationals.
type Epsilon = Double

-- | Derive 'Pitch_Type' from 'Pitch'.
pitch_type :: Pitch i -> Pitch_Type
pitch_type = either (const Pitch_Cents) (const Pitch_Ratio)

scale_name :: Scale i -> String
scale_name (nm,_,_,_) = nm

-- | Text description of scale.
scale_description :: Scale i -> String
scale_description (_,d,_,_) = d

-- | The degree of the scale (number of 'Pitch'es).
scale_degree :: Scale i -> Int
scale_degree (_,_,n,_) = n

-- | The 'Pitch'es at 'Scale'.
scale_pitches :: Scale i -> [Pitch i]
scale_pitches (_,_,_,p) = p

-- | The last 'Pitch' element of the scale (ie. the /ocatve/).
scale_octave :: Scale i -> Maybe (Pitch i)
scale_octave (_,_,_,s) =
    case s of
      [] -> Nothing
      _ -> Just (last s)

-- | Is 'scale_octave' perfect, ie. 'Ratio' of @2@ or 'Cents' of
-- @1200@.
perfect_octave :: Integral i => Scale i -> Bool
perfect_octave s = scale_octave s `elem` [Just (Right 2),Just (Left 1200)]

-- | A pair giving the number of 'Cents' and number of 'Ratio' pitches.
pitch_representations :: Integral t => [Pitch i] -> (t,t)
pitch_representations =
    let f (l,r) p = case p of
                      Left _ -> (l + 1,r)
                      Right _ -> (l,r + 1)
    in foldl f (0,0)

-- | If scale is uniform, give type.
uniform_pitch_type :: [Pitch i] -> Maybe Pitch_Type
uniform_pitch_type p =
    case pitch_representations p :: (Int,Int) of
      (0,_) -> Just Pitch_Ratio
      (_,0) -> Just Pitch_Cents
      _ -> Nothing

-- | The predominant type of the pitches for 'Scale'.
pitch_type_predominant :: [Pitch i] -> Pitch_Type
pitch_type_predominant p =
    let (c,r) = pitch_representations p :: (Int,Int)
    in if c >= r then Pitch_Cents else Pitch_Ratio

-- | Are all pitches of the same type.
is_scale_uniform :: Scale i -> Bool
is_scale_uniform = isJust . uniform_pitch_type . scale_pitches

-- | Pitch as 'T.Cents', conversion by 'T.ratio_to_cents' if necessary.
pitch_cents :: Integral i => Pitch i -> T.Cents
pitch_cents p =
    case p of
      Left c -> c
      Right r -> T.ratio_to_cents r

-- | Pitch as 'Rational', conversion by 'T.reconstructed_ratio' if
-- necessary, hence /epsilon/.
pitch_ratio :: Epsilon -> Pitch Integer -> Rational
pitch_ratio epsilon p =
    case p of
      Left c -> T.reconstructed_ratio epsilon c
      Right r -> r

-- | Make scale pitches uniform, conforming to the most promininent
-- pitch type.
scale_uniform :: Epsilon -> Scale Integer -> Scale Integer
scale_uniform epsilon (nm,d,n,p) =
    case pitch_type_predominant p of
      Pitch_Cents -> (nm,d,n,map (Left . pitch_cents) p)
      Pitch_Ratio -> (nm,d,n,map (Right . pitch_ratio epsilon) p)

-- | Scale as list of 'T.Cents' (ie. 'pitch_cents') with @0@ prefix.
scale_cents :: Integral i => Scale i -> [T.Cents]
scale_cents s = 0 : map pitch_cents (scale_pitches s)

-- | 'map' 'round' of 'scale_cents'.
scale_cents_i :: Integral i => Scale i -> [i]
scale_cents_i = map round . scale_cents

-- | Scale as list of 'Rational' (ie. 'pitch_ratio') with @1@ prefix.
scale_ratios :: Epsilon -> Scale Integer -> [Rational]
scale_ratios epsilon s = 1 : map (pitch_ratio epsilon) (scale_pitches s)

-- | Require that 'Scale' be uniformlay of 'Ratio's.
scale_ratios_req :: Integral i => Scale i -> [Ratio i]
scale_ratios_req =
    let err = error "scale_ratios_req"
    in (1 :) . map (fromMaybe err . T.fromRight) . scale_pitches

-- | Translate 'Scale' to 'T.Tuning'.  If 'Scale' is uniformly
-- rational, 'T.Tuning' is rational, else 'T.Tuning' is in 'T.Cents'.
-- 'Epsilon' is used to recover the 'Rational' octave if required.
scale_to_tuning :: Epsilon -> Scale Integer -> T.Tuning
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
tuning_to_scale :: (String,String) -> T.Tuning -> Scale Integer
tuning_to_scale (nm,dsc) (T.Tuning p o) =
    let n = either length length p
        p' = either (map Right . tail) (map Left . tail) p ++ [Right o]
    in (nm,dsc,n,p')

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
parse_pitch :: (Read i,Integral i) => String -> Pitch i
parse_pitch p =
    if '.' `elem` p
    then Left (T.read_fractional_allow_trailing_point_err p)
    else Right (T.read_ratio_with_div_err p)

-- | Pitch lines may contain commentary.
parse_pitch_ln :: (Read i, Integral i) => String -> Pitch i
parse_pitch_ln x =
    case words x of
      p:_ -> parse_pitch p
      _ -> error (show ("parse_pitch_ln",words x))

-- | Parse @.scl@ file.
parse_scl :: (Read i, Integral i) => String -> String -> Scale i
parse_scl nm s =
    case filter_comments (lines (T.filter_cr s)) of
      t:n:p -> let scl = (nm,T.delete_trailing_whitespace t,T.read_err n,map parse_pitch_ln p)
               in scale_verify_err scl
      _ -> error "parse"

-- * IO

-- | Read the environment variable @SCALA_SCL_DIR@, which is a
-- sequence of directories used to locate scala files on.
--
-- > setEnv "SCALA_DIST_DIR" "/home/rohan/data/scala/83/scl"
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
-- > scl_resolve_name "/home/rohan/data/scala/83/scl/young-lm_piano.scl"
-- > scl_resolve_name "/home/rohan/data/scala/83/scl/unknown-tuning.scl"
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
scl_load :: (Read i, Integral i) => String -> IO (Scale i)
scl_load nm = do
  fn <- scl_resolve_name nm
  s <- T.read_file_iso_8859_1 fn
  return (parse_scl (takeBaseName nm) s)

-- | 'scale_to_tuning' of 'scl_load'.
scl_load_tuning :: Epsilon -> String -> IO T.Tuning
scl_load_tuning epsilon = fmap (scale_to_tuning epsilon) . scl_load

{- | Load all @.scl@ files at /dir/.

> dir <- scl_get_dir
> dir == ["/home/rohan/data/scala/83/scl","/home/rohan/sw/hmt/data/scl"]
> let [scl_83_dir,ext_dir] = dir
> db <- scl_load_dir scl_83_dir
> length db == 4557
> length (filter ((== 0) . scale_degree) db) == 0
> length (filter (== Just (Right 2)) (map scale_octave db)) == 3911
> length (filter is_scale_uniform db) == 2723

> let na = filter (not . T.is_ascending . scale_cents) db
> length na == 121
> mapM_ (putStrLn . unlines . scale_stat) na

> import qualified Music.Theory.List as T
> import Sound.SC3.Plot
> plot_p2_stp [T.histogram (map scale_degree db)]

> import Data.List

> let r = ["Xenakis's Byzantine Liturgical mode, 5 + 19 + 6 parts"
>         ,"Xenakis's Byzantine Liturgical mode, 12 + 11 + 7 parts"
>         ,"Xenakis's Byzantine Liturgical mode, 7 + 16 + 7 parts"]
> in filter (isInfixOf "Xenakis") (map scale_description db) == r

> let r = ["LaMonte Young, tuning of For Guitar '58. 1/1 March '92, inv.of Mersenne lute 1"
>         ,"LaMonte Young's Well-Tuned Piano"]
> in filter (isInfixOf "LaMonte Young") (map scale_description db) == r

> length (filter (not . perfect_octave) db) == 641

-}
scl_load_dir :: (Read i, Integral i) => FilePath -> IO [Scale i]
scl_load_dir d = T.dir_subset [".scl"] d >>= mapM scl_load

-- | Load Scala data base at 'scl_get_dir'.
--
-- > db <- scl_load_db
-- > mapM_ (putStrLn.unlines.scale_stat) (filter (not . perfect_octave) db)
scl_load_db :: (Read i, Integral i) => IO [Scale i]
scl_load_db = do
  dir <- scl_get_dir
  r <- mapM scl_load_dir dir
  return (concat r)

-- * PP

scale_stat :: (Integral i,Show i) => Scale i -> [String]
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
pitch_pp :: (Integral i,Show i) => Pitch i -> String
pitch_pp p =
    case p of
      Left c -> show c
      Right r -> show (numerator r) ++ "/" ++ show (denominator r)

-- | Pretty print 'Scale' in @Scala@ format.
--
-- > s <- scl_load "et19"
-- > s <- scl_load "young-lm_piano"
-- > putStr $ unlines $ scale_pp s
scale_pp :: (Integral i,Show i) => Scale i -> [String]
scale_pp (nm,dsc,k,p) =
    ["! " ++ nm ++ ".scl"
    ,"!"
    ,dsc
    ,show k
    ,"!"] ++ map pitch_pp p

-- * DIST

-- | Load file from @scala@ distribution directory, given at @SCALA_DIST_DIR@.
--
-- > s <- load_dist_file "intnam.par"
-- > length s == 473
load_dist_file :: FilePath -> IO [String]
load_dist_file nm = do
  d <- getEnv "SCALA_DIST_DIR"
  fmap lines (readFile (d </> nm))
