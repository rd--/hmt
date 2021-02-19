{- | Scala "keyboard mapping" files (.kbm) and related data structure.

<http://www.huygens-fokker.org/scala/help.htm#mappings>
-}
module Music.Theory.Tuning.Scala.KBM where

import Data.List {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.Directory as Directory {- hmt -}
import qualified Music.Theory.List as List {- hmt -}
import qualified Music.Theory.Pitch as Pitch {- hmt -}
import qualified Music.Theory.Tuning as Tuning {- hmt -}
import qualified Music.Theory.Tuning.Scala as Scala {- hmt -}

{- | Scala keyboard mapping

(sz,(m0,mN),mC,(mF,f),o,m)

- sz      = size of map, the pattern repeats every so many keys
- (m0,mN) = the first and last midi note numbers to retune
- mC      = the middle note where the first entry of the mapping is mapped to
- (mF,f)  = the reference midi-note for which a frequency is given, ie. (69,440)
- o       = scale degree to consider as formal octave
- m       = mapping, numbers represent scale degrees mapped to keys, Nothing indicates no mapping

-}
type KBM = (Int,(Int,Int),Int,(Int,Double),Int,[Maybe Int])

-- | Pretty-printer for scala .kbm file.
kbm_pp :: KBM -> String
kbm_pp (sz,(m0,mN),mC,(mF,f),o,m) =
  unlines
  [printf "size = %d" sz
  ,printf "note-range = (%d,%d)" m0 mN
  ,printf "note-center = %d" mC
  ,printf "note-reference = (%d,%f)" mF f
  ,printf "formal-octave = %d" o
  ,printf "map = [%s] #%d" (intercalate "," (map (maybe "x" show) m)) (length m)]

-- | Is /mnn/ in range?
kbm_in_rng :: KBM -> Int -> Bool
kbm_in_rng (_,(m0,mN),_,_,_,_) mnn = mnn >= m0 && mnn <= mN

-- | Is /kbm/ linear?, ie. is size zero? (formal-octave may or may not be zero)
kbm_is_linear :: KBM -> Bool
kbm_is_linear (sz,_,_,_,_o,_) = sz == 0 -- && o == 0

{- | Given kbm and midi-note-number lookup (octave,scale-degree).

> k <- kbm_load_dist "example.kbm" -- 12-tone scale
> k <- kbm_load_dist "a440.kbm" -- linear
> k <- kbm_load_dist "white.kbm" -- 7-tone scale on white notes
> k <- kbm_load_dist "black.kbm" -- 5-tone scale on black notes
> k <- kbm_load_dist "128.kbm"

> map (kbm_lookup k) [48 .. 72]

-}
kbm_lookup :: KBM -> Int -> Maybe (Int,Int)
kbm_lookup kbm mnn =
  if not (kbm_in_rng kbm mnn)
  then Nothing
  else if kbm_is_linear kbm
       then Just (0,mnn)
       else let (sz,(_m0,_mN),mC,(_mF,_f),_o,m) = kbm
                (oct,ix) = ((mnn - mC) `divMod` sz)
            in maybe Nothing (\dgr -> Just (oct,dgr)) (m !! ix)

-- | Return the triple (mF,kbm_lookup k mF,f).  The lookup for mF is not-nil by definition.
--
-- > kbm_lookup_mF k
kbm_lookup_mF :: KBM -> (Int,(Int,Int),Double)
kbm_lookup_mF k@(_,_,_,(mF,f),_,_) =
  case kbm_lookup k mF of
    Nothing -> error "kbm_lookup_mF?"
    Just r -> (mF,r,f)

-- | Parser for scala .kbm file.
kbm_parse :: String -> KBM
kbm_parse s =
  let f x = case x of
              "x" -> Nothing
              _ -> Just (read x)
      to_m sz = List.pad_right_no_truncate Nothing sz . map f -- _err -- some scala .kbm have |m| > sz?
  in case Scala.filter_comments (lines s) of
       i1:i2:i3:i4:i5:d1:i6:m ->
         let sz = read i1
         in (sz,(read i2,read i3),read i4,(read i5,read d1),read i6,to_m sz m)
       _ -> error "kbm_parse?"

-- | 'kbm_parse' of 'readFile'
kbm_load :: FilePath -> IO KBM
kbm_load = fmap kbm_parse . readFile

{- | 'kbm_parse' of 'Scala.load_dist_file'

> pp fn = kbm_load_dist fn >>= \x -> putStrLn (kbm_pp x)
> pp "example.kbm"
> pp "bp.kbm"
> pp "7.kbm" -- error -- 12/#13
> pp "8.kbm" -- error -- 12/#13
> pp "white.kbm" -- error -- 12/#13
> pp "black.kbm" -- error -- 12/#13
> pp "128.kbm"
> pp "a440.kbm"
> pp "61.kbm"
-}
kbm_load_dist :: FilePath -> IO KBM
kbm_load_dist = fmap kbm_parse . Scala.load_dist_file

-- | Load all .kbm files at directory.
kbm_load_dir_fn :: FilePath -> IO [(FilePath, KBM)]
kbm_load_dir_fn d = do
  fn <- Directory.dir_subset [".kbm"] d
  kbm <- mapM kbm_load fn
  return (zip fn kbm)

{- | Load all .kbm files at scala DIST dir.

> db <- kbm_load_dist_dir_fn
> length db == 41
> x = map (\(fn,(sz,_,_,_,o,m)) -> (System.FilePath.takeFileName fn,sz,length m,o)) db
> filter (\(_,i,j,_) -> i < j) x -- size < map-length
> filter (\(_,i,_,k) -> i == 0 && k == 0) x -- size and formal octave both zero

> map (\(fn,k) -> (System.FilePath.takeFileName fn,kbm_lookup_mF k)) db
-}
kbm_load_dist_dir_fn :: IO [(FilePath, KBM)]
kbm_load_dist_dir_fn = Scala.dist_get_dir >>= kbm_load_dir_fn

{- | Pretty-printer for scala .kbm file.

> m <- kbm_load_dist "7.kbm"
> kbm_parse (kbm_format m) == m
> putStrLn $ kbm_pp m
-}
kbm_format :: KBM -> String
kbm_format (i1,(i2,i3),i4,(i5,d1),i6,m) =
  let from_m = map (maybe "x" show)
  in unlines ([show i1,show i2,show i3,show i4,show i5,show d1,show i6] ++ from_m m)

-- | 'writeFile' of 'kbm_format'
kbm_wr :: FilePath -> KBM -> IO ()
kbm_wr fn = writeFile fn . kbm_format

{- | Standard 12-tone mapping with A=440hz (ie. example.kbm)

> fmap (== kbm_d12_a440) (kbm_load_dist "example.kbm")
> putStrLn $ kbm_pp kbm_d12_a440
-}
kbm_d12_a440 :: KBM
kbm_d12_a440 = (12,(0,127),60,(69,440.0),12,map Just [0 .. 11])

kbm_d12_c256 :: KBM
kbm_d12_c256 = (12,(0,127),60,(60,256.0),12,map Just [0 .. 11])

-- | Given size and note-center calculate relative octave and key
--   number (not scale degree) of the zero entry.
--
-- > map (kbm_m0 12) [59,60,61] == [(-4,1),(-5,0),(-5,11)]
kbm_m0 :: Int -> Int -> (Int,Int)
kbm_m0 sz mC = let (o,r) = mC `quotRem` sz in (negate o,negate r `mod` sz)

-- | Given size and note-center calculate complete octave and key
-- number sequence (ie. for entries 0 - 127).
--
-- > map (zip [0..] . kbm_oct_key_seq 12) [59,60,61]
kbm_oct_key_seq :: Int -> Int -> [(Int,Int)]
kbm_oct_key_seq sz mC =
  let (o0,m0) = kbm_m0 sz mC
      dgr = map (`mod` sz) (take 128 [m0 ..])
      upd o j = if j == 0 then (o + 1,(o + 1,j)) else (o,(o,j))
  in snd (mapAccumL upd (o0 - 1) dgr)

-- | Given KBM and SCL calculate frequency of note-center.
kbm_mC_freq :: KBM -> Scala.Scale -> Double
kbm_mC_freq (sz,(_m0,_mN),mC,(mF,f),_o,m) scl =
  let dist_k = (mF - mC) `mod` sz
      Just dgr = m !! dist_k
      c = Scala.scale_cents scl !! dgr
  in Tuning.cps_shift_cents f (- c)

{-

scl <- Scala.scl_load "young-lm_piano"
scl <- Scala.scl_load "meanquar"
scl <- Scala.scl_load "et12"
kbm = kbm_d12_a440 -- kbm_d12_a440 kbm_d12_c256

mC_f = kbm_mC_freq kbm scl
Pitch.cps_to_fmidi mC_f

-}
