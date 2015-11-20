-- | Parser for the Scala scale file format.  See
-- <http://www.huygens-fokker.org/scala/scl_format.html> for details.
-- This module succesfully parses all 4557 scales in v.83 of the scale
-- library.
module Music.Theory.Tuning.Scala where

import Control.Monad {- base -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import System.Environment {- base -}
import System.FilePath {- filepath -}

import qualified Music.Theory.Directory as T {- hmt -}
import qualified Music.Theory.Function as T {- hmt -}
import qualified Music.Theory.String as T {- hmt -}
import qualified Music.Theory.Tuning as T {- hmt -}

-- * Types

-- | A @.scl@ pitch is either in 'Cents' or is a 'Ratio'.
data Pitch_Type = Pitch_Cents | Pitch_Ratio deriving (Eq,Show)

-- | A @.scl@ pitch is either in 'Cents' or is a 'Ratio'.
type Pitch i = Either T.Cents (Ratio i)

-- | A scale has a name, a description, a degree, and a list of 'Pitch'es.
type Scale i = (String,String,i,[Pitch i])

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
scale_degree :: Scale i -> i
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

-- | A pair giving the number of 'Cents' and number of 'Ratio' pitches
-- at 'Scale'.
scale_pitch_representations :: Integral t => Scale i -> (t,t)
scale_pitch_representations s =
    let f (l,r) p = case p of
                      Left _ -> (l + 1,r)
                      Right _ -> (l,r + 1)
    in foldl f (0,0) (scale_pitches s)

-- | If scale is uniform, give type.
scale_type :: Scale i -> Maybe Pitch_Type
scale_type s =
    case scale_pitch_representations s :: (Int,Int) of
      (0,_) -> Just Pitch_Ratio
      (_,0) -> Just Pitch_Cents
      _ -> Nothing

-- | Are all pitches of the same type.
is_scale_uniform :: Scale i -> Bool
is_scale_uniform = isJust . scale_type

-- | Pitch as 'T.Cents', conversion by 'T.to_cents_r' if necessary.
pitch_cents :: Pitch Integer -> T.Cents
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
scale_uniform epsilon s =
    let (nm,d,n,p) = s
        (c,r) = scale_pitch_representations s :: (Int,Int)
    in if c >= r
       then (nm,d,n,map (Left . pitch_cents) p)
       else (nm,d,n,map (Right . pitch_ratio epsilon) p)

-- | Scale as list of 'T.Cents' (ie. 'pitch_cents') with @0@ prefix.
scale_cents :: Scale Integer -> [T.Cents]
scale_cents s = 0 : map pitch_cents (scale_pitches s)

-- | Scale as list of 'Rational' (ie. 'pitch_ratio') with @1@ prefix.
scale_ratios :: Epsilon -> Scale Integer -> [Rational]
scale_ratios epsilon s = 1 : map (pitch_ratio epsilon) (scale_pitches s)

-- * Parser

-- | Comment lines begin with @!@.
is_comment :: String -> Bool
is_comment x =
    case x of
      '!':_ -> True
      _ -> False

-- | Remove to end of line @!@ comments.
remove_eol_comments :: String -> String
remove_eol_comments = takeWhile (/= '!')

-- | Remove comments and null lines.
--
-- > filter_comments ["!a","b","","c"] == ["b","c"]
filter_comments :: [String] -> [String]
filter_comments =
    map remove_eol_comments .
    filter (not . T.predicate_any [is_comment,null])

-- | Delete trailing @.@, 'read' fails for @700.@.
delete_trailing_point :: String -> String
delete_trailing_point s =
    case reverse s of
      '.':s' -> reverse s'
      _ -> s

-- | Pitches are either cents (with decimal point) or ratios (with @/@).
--
-- > map parse_pitch ["700.0","3/2","2"] == [Left 700,Right (3/2),Right 2]
parse_pitch :: (Read i,Integral i) => String -> Pitch i
parse_pitch p =
    if '.' `elem` p
    then Left (read (delete_trailing_point p))
    else case break (== '/') p of
             (n,'/':d) -> Right (read n % read d)
             _ -> Right (read p % 1)

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
      t:n:p -> (nm,T.delete_trailing_whitespace t,read n,map parse_pitch_ln p)
      _ -> error "parse"

-- * IO

-- | Read the environment variable @SCALA_SCL_DIR@.
scl_get_dir :: IO String
scl_get_dir = getEnv "SCALA_SCL_DIR"

-- | Lookup the @SCALA_SCL_DIR@ environment variable, which must exist, and derive the filepath.
-- It is an error if the name has a file extension.
--
-- > scl_derive_filename "young-lm_piano"
scl_derive_filename :: FilePath -> IO FilePath
scl_derive_filename nm = do
  dir <- scl_get_dir
  when (null dir) (error "scl_derive_filename: SCALA_SCL_DIR: nil")
  when (hasExtension nm) (error "scl_derive_filename: name has extension")
  return (dir </> nm <.> "scl")

-- | If the name is an absolute file path and has a @.scl@ extension,
-- then return it, else run 'scl_derive_filename'.
--
-- > scl_resolve_name "young-lm_piano"
-- > scl_resolve_name "/home/rohan/data/scala/83/scl/young-lm_piano.scl"
scl_resolve_name :: String -> IO FilePath
scl_resolve_name nm =
    if isAbsolute nm && takeExtension nm == ".scl"
    then return nm
    else scl_derive_filename nm

-- | Load @.scl@ file, runs 'resolve_scl'.
--
-- > s <- scl_load "xenakis_chrom"
-- > scale_pitch_representations s == (6,1)
-- > scale_ratios 1e-3 s == [1,21/20,29/23,179/134,280/187,11/7,100/53,2]
scl_load :: (Read i, Integral i) => FilePath -> IO (Scale i)
scl_load nm = do
  fn <- scl_resolve_name nm
  s <- T.read_file_iso_8859_1 fn
  return (parse_scl (takeBaseName nm) s)

{- | Load all @.scl@ files at /dir/.

> dir <- scl_get_dir
> dir == "/home/rohan/data/scala/83/scl"
> db <- scl_load_dir dir
> length db == 4557
> length (filter ((== 0) . scale_degree) db) == 0
> length (filter (== Just (Right 2)) (map scale_octave db)) == 3911
> length (filter is_scale_uniform db) == 2723

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

-- > db <- scl_load_db
-- > mapM_ (putStrLn.unlines.scale_stat) (filter (not . perfect_octave) db)
scl_load_db :: (Read i, Integral i) => IO [Scale i]
scl_load_db = do
  dir <- scl_get_dir
  scl_load_dir dir

-- * PP

scale_stat :: (Integral i,Show i) => Scale i -> [String]
scale_stat s =
    ["scale-name        : " ++ scale_name s
    ,"scale-description : " ++ scale_description s
    ,"scale-degree      : " ++ show (scale_degree s)
    ,"scale-type        : " ++ maybe "non-uniform" show (scale_type s)
    ,"perfect-octave    : " ++ show (perfect_octave s)]
