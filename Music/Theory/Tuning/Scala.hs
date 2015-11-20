-- | Parser for the Scala scale file format.  See
-- <http://www.huygens-fokker.org/scala/scl_format.html> for details.
-- This module succesfully parses all 4557 scales in v.83 of the scale
-- library.
module Music.Theory.Tuning.Scala where

import qualified Codec.Binary.UTF8.String as U {- utf8-string -}
import Control.Monad {- base -}
import qualified Data.ByteString as B {- bytestring -}
import Data.List {- base -}
import Data.Ratio {- base -}
import System.Directory {- directory -}
import System.Environment {- base -}
import System.FilePath {- filepath -}

import qualified Music.Theory.Tuning as T {- hmt -}

-- | A @.scl@ pitch is either in 'Cents' or is a 'Ratio'.
type Pitch i = Either T.Cents (Ratio i)

-- | A scale has a description, a degree, and a list of 'Pitch'es.
type Scale i = (String,i,[Pitch i])

-- | Text description of scale.
scale_description :: Scale i -> String
scale_description (d,_,_) = d

-- | The degree of the scale (number of 'Pitch'es).
scale_degree :: Scale i -> i
scale_degree (_,n,_) = n

-- | The 'Pitch'es at 'Scale'.
scale_pitches :: Scale i -> [Pitch i]
scale_pitches (_,_,p) = p

-- | The last 'Pitch' element of the scale (ie. the /ocatve/).
scale_octave :: Scale i -> Maybe (Pitch i)
scale_octave (_,_,s) =
    case s of
      [] -> Nothing
      _ -> Just (last s)

-- | Is 'scale_octave' perfect, ie. 'Ratio' of @2@ or 'Cents' of
-- @1200@.
perfect_octave :: Integral i => Scale i -> Bool
perfect_octave s = scale_octave s `elem` [Just (Right 2),Just (Left 1200)]

-- | A pair giving the number of 'Cents' and number of 'Ratio' pitches
-- at 'Scale'.
scale_pitch_representations :: (Integral t) => Scale i -> (t,t)
scale_pitch_representations s =
    let f (l,r) p = case p of
                      Left _ -> (l + 1,r)
                      Right _ -> (l,r + 1)
    in foldl f (0,0) (scale_pitches s)

-- | Pitch as 'T.Cents', conversion by 'T.to_cents_r' if necessary.
pitch_cents :: Pitch Integer -> T.Cents
pitch_cents p =
    case p of
      Left c -> c
      Right r -> T.ratio_to_cents r

type Epsilon = Double

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
    let (d,n,p) = s
        (c,r) = scale_pitch_representations s :: (Int,Int)
    in if c >= r
       then (d,n,map (Left . pitch_cents) p)
       else (d,n,map (Right . pitch_ratio epsilon) p)

-- | Scale as list of 'T.Cents' (ie. 'pitch_cents') with @0@ prefix.
scale_cents :: Scale Integer -> [T.Cents]
scale_cents s = 0 : map pitch_cents (scale_pitches s)

-- | Scale as list of 'Rational' (ie. 'pitch_ratio') with @1@ prefix.
scale_ratios :: Epsilon -> Scale Integer -> [Rational]
scale_ratios epsilon s = 1 : map (pitch_ratio epsilon) (scale_pitches s)

-- | Comment lines being with @!@.
comment_p :: String -> Bool
comment_p x =
    case x of
      '!':_ -> True
      _ -> False

-- | Remove @\r@.
filter_cr :: String -> String
filter_cr = filter (not . (==) '\r')

-- | Logical /or/ of list of predicates.
p_or :: [a -> Bool] -> a -> Bool
p_or p x =
    case p of
      [] -> False
      f:p' -> f x || p_or p' x

-- | Remove to end of line @!@ comments.
remove_eol_comments :: String -> String
remove_eol_comments = takeWhile (/= '!')

-- | Remove comments and null lines.
--
-- > filter_comments ["!a","b","","c"] == ["b","c"]
filter_comments :: [String] -> [String]
filter_comments = map remove_eol_comments .
                  filter (not . p_or [comment_p,null])

-- | Delete trailing @.@, 'read' fails for @700.@.
delete_trailing_point :: String -> String
delete_trailing_point s =
    case reverse s of
      '.':s' -> reverse s'
      _ -> s

-- | Pitches are either cents (with decimal point) or ratios (with @/@).
--
-- > map pitch ["700.0","3/2","2"] == [Left 700,Right (3/2),Right 2]
pitch :: (Read i,Integral i) => String -> Pitch i
pitch p =
    if '.' `elem` p
    then Left (read (delete_trailing_point p))
    else case break (== '/') p of
             (n,'/':d) -> Right (read n % read d)
             _ -> Right (read p % 1)

-- | Pitch lines may contain commentary.
pitch_ln :: (Read i, Integral i) => String -> Pitch i
pitch_ln x =
    case words x of
      p:_ -> pitch p
      _ -> error (show ("pitch",words x))

-- | Parse @.scl@ file.
parse :: (Read i, Integral i) => String -> Scale i
parse s =
    case filter_comments (lines (filter_cr s)) of
      t:n:p -> (t,read n,map pitch_ln p)
      _ -> error "parse"

get_scl_dir :: IO String
get_scl_dir = getEnv "SCALA_SCL_DIR"

-- | Lookup the @SCALA_SCL_DIR@ environment variable, which must exist, and derive the filepath.
-- It is an error if the name has a file extension.
--
-- > derive_scl_filename "young-lm_piano"
derive_scl_filename :: FilePath -> IO FilePath
derive_scl_filename nm = do
  dir <- get_scl_dir
  when (null dir) (error "derive_scl_filename: SCALA_SCL_DIR: nil")
  when (hasExtension nm) (error "derive_scl_filename: name has extension")
  return (dir </> nm <.> "scl")

-- | If the name is an absolute file path and has a @.scl@ extension,
-- then return it, else run 'derive_scl_filename'.
--
-- > resolve_scl_name "young-lm_piano"
-- > resolve_scl_name "/home/rohan/data/scala/83/scl/young-lm_piano.scl"
resolve_scl_name :: String -> IO FilePath
resolve_scl_name nm =
    if isAbsolute nm && takeExtension nm == ".scl"
    then return nm
    else derive_scl_filename nm

-- | Load @.scl@ file, runs 'resolve_scl'.
--
-- > s <- load "xenakis_chrom"
-- > scale_pitch_representations s == (6,1)
-- > scale_ratios 1e-3 s == [1,21/20,29/23,179/134,280/187,11/7,100/53,2]
load :: (Read i, Integral i) => FilePath -> IO (Scale i)
load nm = do
  fn <- resolve_scl_name nm
  b <- B.readFile fn
  let s = U.decode (B.unpack b)
  return (parse s)

-- | Subset of files in /dir/ with an extension in /ext/.
--
-- > dir <- get_scl_dir
-- > nm <- dir_subset [".scl"] dir
-- > length nm == 4557
dir_subset :: [String] -> FilePath -> IO [FilePath]
dir_subset ext dir = do
  let f nm = takeExtension nm `elem` ext
  c <- getDirectoryContents dir
  return (map (dir </>) (sort (filter f c)))

{- | Load all @.scl@ files at /dir/.

> dir <- get_scl_dir
> dir == "/home/rohan/data/scala/83/scl"
> db <- load_dir dir
> length db == 4557
> length (filter ((== 0) . scale_degree) db) == 0
> length (filter (== Just (Right 2)) (map scale_octave db)) == 3911

> import qualified Music.Theory.List as T
> import Sound.SC3.Plot
> plot_p2_stp [T.histogram (map scale_degree db)]

> let r = ["Xenakis's Byzantine Liturgical mode, 5 + 19 + 6 parts"
>         ,"Xenakis's Byzantine Liturgical mode, 12 + 11 + 7 parts"
>         ,"Xenakis's Byzantine Liturgical mode, 7 + 16 + 7 parts"]
> in filter (isInfixOf "Xenakis") (map scale_description db) == r

> length (filter (not . perfect_octave) db) == 641

> mapM_ (putStrLn.scale_description) (filter (not . perfect_octave) db)

-}
load_dir :: (Read i, Integral i) => FilePath -> IO [Scale i]
load_dir d = dir_subset [".scl"] d >>= mapM load

-- Local Variables:
-- truncate-lines:t
-- End:
