-- | Parser for the Scala scale file format.  See
-- <http://www.huygens-fokker.org/scala/scl_format.html> for details.
-- This module succesfully parses all 4115 scales in v.77 of the scale
-- library.
module Music.Theory.Tuning.Scala where

import qualified Codec.Binary.UTF8.String as U {- utf8-string -}
import qualified Data.ByteString as B {- bytestring -}
import Data.List
import Data.Ratio
import qualified Music.Theory.Tuning as T
import System.Directory {- directory -}
import System.FilePath {- filepath -}

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
      Right r -> T.to_cents_r r

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
      f:p' -> if f x then True else p_or p' x

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

-- | Load @.scl@ file.
--
-- > s <- load "/home/rohan/opt/scala/scl/xenakis_chrom.scl"
-- > scale_pitch_representations s == (6,1)
-- > scale_ratios 1e-3 s == [1,21/20,29/23,179/134,280/187,11/7,100/53,2]
load :: (Read i, Integral i) => FilePath -> IO (Scale i)
load fn = do
  b <- B.readFile fn
  let s = U.decode (B.unpack b)
  return (parse s)

-- | Subset of files in /dir/ with an extension in /ext/.
dir_subset :: [String] -> FilePath -> IO [FilePath]
dir_subset ext dir = do
  let f nm = takeExtension nm `elem` ext
  c <- getDirectoryContents dir
  return (map (dir </>) (sort (filter f c)))

-- | Load all @.scl@ files at /dir/.
--
-- > db <- load_dir "/home/rohan/opt/scala/scl"
-- > length db == 4115
-- > length (filter ((== 0) . scale_degree) db) == 1
-- > length (filter (== Just (Right 2)) (map scale_octave db)) == 3562
-- > nub (sort (map scale_degree db)) == [0,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,74,75,77,78,79,80,81,84,87,88,90,91,92,95,96,99,100,101,105,110,112,117,118,130,140,171,180,271,311,342,366,441,612]
--
-- > filter (isInfixOf "Xenakis") (map scale_description db)
-- > map scale_description (filter (not . perfect_octave) db)
load_dir :: (Read i, Integral i) => FilePath -> IO [Scale i]
load_dir d = dir_subset [".scl"] d >>= mapM load

-- Local Variables:
-- truncate-lines:t
-- End:
