{- | Scala "keyboard mapping" files (.kbm) and related data structure.

<http://www.huygens-fokker.org/scala/help.htm#mappings>
-}
module Music.Theory.Tuning.Scala.KBM where

import qualified Music.Theory.Directory as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Tuning.Scala as T {- hmt -}

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
      to_m sz = T.pad_right_no_truncate Nothing sz . map f -- _err -- some scala .kbm have |m| > sz?
  in case T.filter_comments (lines s) of
       i1:i2:i3:i4:i5:d1:i6:m ->
         let sz = read i1
         in (sz,(read i2,read i3),read i4,(read i5,read d1),read i6,to_m sz m)
       _ -> error "kbm_parse?"

-- | 'kbm_parse' of 'readFile'
kbm_load :: FilePath -> IO KBM
kbm_load = fmap kbm_parse . readFile

{- | 'kbm_parse' of 'T.load_dist_file'

> kbm_load_dist "example.kbm"
> kbm_load_dist "bp.kbm"
> kbm_load_dist "7.kbm" -- error
> kbm_load_dist "8.kbm" -- error
> kbm_load_dist "white.kbm" -- error
> kbm_load_dist "black.kbm" -- error
> kbm_load_dist "128.kbm"
> kbm_load_dist "a440.kbm"
> kbm_load_dist "61.kbm"
-}
kbm_load_dist :: FilePath -> IO KBM
kbm_load_dist = fmap kbm_parse . T.load_dist_file

kbm_load_dir_fn :: FilePath -> IO [(FilePath, KBM)]
kbm_load_dir_fn d = do
  fn <- T.dir_subset [".kbm"] d
  kbm <- mapM kbm_load fn
  return (zip fn kbm)

{- | Load all .kbm files at scala DIST dir.

> db <- kbm_load_dist_dir_fn
> x = map (\(fn,(sz,_,_,_,o,m)) -> (System.FilePath.takeFileName fn,sz,length m,o)) db
> filter (\(_,i,j,_) -> i < j) x
> filter (\(_,i,_,k) -> i == 0 && k == 0) x

> map (\(fn,k) -> (System.FilePath.takeFileName fn,kbm_lookup_mF k)) db
-}
kbm_load_dist_dir_fn :: IO [(FilePath, KBM)]
kbm_load_dist_dir_fn = T.dist_get_dir >>= kbm_load_dir_fn

{- | Pretty-printer for scala .kbm file.

> m <- kbm_load_dist "7.kbm"
> kbm_parse (kbm_pp m) == m
-}
kbm_pp :: KBM -> String
kbm_pp (i1,(i2,i3),i4,(i5,d1),i6,m) =
  let from_m = map (maybe "x" show)
  in unlines ([show i1,show i2,show i3,show i4,show i5,show d1,show i6] ++ from_m m)

-- | 'writeFile' of 'kbm_pp'
kbm_wr :: FilePath -> KBM -> IO ()
kbm_wr fn = writeFile fn . kbm_pp

{- | Standard 12-tone mapping with A=440hz (ie. example.kbm)

> fmap (== kbm_d12_a440) (kbm_load_dist "example.kbm")
-}
kbm_d12_a440 :: KBM
kbm_d12_a440 = (12,(0,127),60,(69,440.0),12,map Just [0 .. 11])
