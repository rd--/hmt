-- | Equal temperament tuning tables.
module Music.Theory.Tuning.ET where

import Data.List {- base -}
import Data.List.Split {- split -}
import Data.Ratio {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.List as T {- hmt -}
import Music.Theory.Pitch {- hmt -}
import Music.Theory.Pitch.Note {- hmt -}
import Music.Theory.Pitch.Spelling.Table {- hmt -}
import Music.Theory.Tuning {- hmt -}

-- | 'octpc_to_pitch' and 'octpc_to_cps'.
octpc_to_pitch_cps_f0 :: (Floating n) => n -> OctPC -> (Pitch,n)
octpc_to_pitch_cps_f0 f0 x = (octpc_to_pitch pc_spell_ks x,octpc_to_cps_f0 f0 x)

-- | 'octpc_to_pitch' and 'octpc_to_cps'.
octpc_to_pitch_cps :: (Floating n) => OctPC -> (Pitch,n)
octpc_to_pitch_cps = octpc_to_pitch_cps_f0 440

-- | 12-tone equal temperament table equating 'Pitch' and frequency
-- over range of human hearing, where @A4@ has given frequency.
--
-- > tbl_12et_f0 415
tbl_12et_f0 :: Double -> [(Pitch,Double)]
tbl_12et_f0 f0 =
    let z = [(o,pc) | o <- [0..10], pc <- [0..11]]
    in map (octpc_to_pitch_cps_f0 f0) z

-- | 'tbl_12et_f0' @440@hz.
--
-- > length tbl_12et == 132
-- > minmax (map (round . snd) tbl_12et) == (16,31609)
tbl_12et :: [(Pitch,Double)]
tbl_12et = tbl_12et_f0 440

-- | 24-tone equal temperament variant of 'tbl_12et_f0'.
tbl_24et_f0 :: Double -> [(Pitch,Double)]
tbl_24et_f0 f0 =
    let f x = let p = fmidi_to_pitch_err pc_spell_ks x
                  p' = pitch_rewrite_threequarter_alteration p
              in (p',fmidi_to_cps_f0 f0 x)
    in map f [12,12.5 .. 143.5]

-- | 'tbl_24et_f0' @440@.
--
-- > length tbl_24et == 264
-- > T.minmax (map (round . snd) tbl_24et) == (16,32535)
tbl_24et :: [(Pitch,Double)]
tbl_24et = tbl_24et_f0 440

-- | Given an @ET@ table (or like) find bounds of frequency.
--
-- import qualified Music.Theory.Tuple as T
--
-- > let r = Just (T.t2_map octpc_to_pitch_cps ((3,11),(4,0)))
-- > in bounds_et_table tbl_12et 256 == r
bounds_et_table :: Ord s => [(t,s)] -> s -> Maybe ((t,s),(t,s))
bounds_et_table = T.find_bounds True (compare . snd)

-- | 'bounds_et_table' of 'tbl_12et'.
--
-- > map bounds_12et_tone (hsn 17 55)
bounds_12et_tone :: Double -> Maybe ((Pitch,Double),(Pitch,Double))
bounds_12et_tone = bounds_et_table tbl_12et

-- | Tuple indicating nearest 'Pitch' to /frequency/ with @ET@
-- frequency, and deviation in hertz and 'Cents'.
--
-- (cps,nearest-pitch,cps-of-nearest-pitch,cps-deviation,cents-deviation)
type HS_R p = (Double,p,Double,Double,Cents)

-- | /n/-decimal places.
--
-- > ndp 3 (1/3) == "0.333"
ndp :: Int -> Double -> String
ndp = printf "%.*f"

-- | Pretty print 'HS_R'.
hs_r_pp :: (p -> String) -> Int -> HS_R p -> [String]
hs_r_pp pp n (f,p,pf,fd,c) =
    let dp = ndp n
    in [dp f
       ,pp p
       ,dp pf
       ,dp fd
       ,dp c]

hs_r_pitch_pp :: Int -> HS_R Pitch -> [String]
hs_r_pitch_pp = hs_r_pp pitch_pp

{- | Form 'HS_R' for /frequency/ by consulting table.

> let {f = 256
>     ;f' = octpc_to_cps (4,0)
>     ;r = (f,Pitch C Natural 4,f',f-f',fratio_to_cents (f/f'))}
> in nearest_et_table_tone tbl_12et 256 == r

-}
nearest_et_table_tone :: [(p,Double)] -> Double -> HS_R p
nearest_et_table_tone tbl f =
    case bounds_et_table tbl f of
      Nothing -> error "nearest_et_table_tone: no bounds?"
      Just ((lp,lf),(rp,rf)) ->
          let ld = f - lf
              rd = f - rf
          in if abs ld < abs rd
             then (f,lp,lf,ld,fratio_to_cents (f/lf))
             else (f,rp,rf,rd,fratio_to_cents (f/rf))

-- | 'nearest_et_table_tone' for 'tbl_12et'.
nearest_12et_tone :: Double -> HS_R Pitch
nearest_12et_tone = nearest_et_table_tone tbl_12et

-- | 'nearest_et_table_tone' for 'tbl_24et'.
--
-- > let r = "55.0 A1 55.0 0.0 0.0"
-- > in unwords (hs_r_pitch_pp 1 (nearest_24et_tone 55)) == r
nearest_24et_tone :: Double -> HS_R Pitch
nearest_24et_tone = nearest_et_table_tone tbl_24et

-- * 72ET

-- | Monzo 72-edo HEWM notation.  The domain is (-9,9).
-- <http://www.tonalsoft.com/enc/number/72edo.aspx>
--
-- > let r = ["+",">","^","#<","#-","#","#+","#>","#^"]
-- > in map alteration_72et_monzo [1 .. 9] == r
--
-- > let r = ["-","<","v","b>","b+","b","b-","b<","bv"]
-- > in map alteration_72et_monzo [-1,-2 .. -9] == r
alteration_72et_monzo :: Integral n => n -> String
alteration_72et_monzo n =
    let spl = splitOn ","
        asc = spl ",+,>,^,#<,#-,#,#+,#>,#^"
        dsc = spl ",-,<,v,b>,b+,b,b-,b<,bv"
    in case compare n 0 of
         LT -> genericIndex dsc (- n)
         EQ -> ""
         GT -> genericIndex asc n

-- | Given a midi note number and @1/6@ deviation determine 'Pitch''
-- and frequency.
--
-- > let {f = pitch'_pp . fst . pitch_72et
-- >     ;r = "C4 C+4 C>4 C^4 C#<4 C#-4 C#4 C#+4 C#>4 C#^4"}
-- > in unwords (map f (zip (repeat 60) [0..9])) == r
--
-- > let {f = pitch'_pp . fst . pitch_72et
-- >     ;r = "A4 A+4 A>4 A^4 Bb<4 Bb-4 Bb4 Bb+4 Bb>4 Bv4"}
-- > in unwords (map f (zip (repeat 69) [0..9]))
--
-- > let {f = pitch'_pp . fst . pitch_72et
-- >     ;r = "Bb4 Bb+4 Bb>4 Bv4 B<4 B-4 B4 B+4 B>4 B^4"}
-- > in unwords (map f (zip (repeat 70) [0..9])) == r
pitch_72et :: (Midi,Int) -> (Pitch_R,Double)
pitch_72et (x,n) =
    let p = midi_to_pitch_ks x
        t = note p
        a = alteration p
        (t',n') = case a of
                    Flat -> if n < (-3) then (pred t,n + 6) else (t,n - 6)
                    Natural -> (t,n)
                    Sharp -> if n > 3 then (succ t,n - 6) else (t,n + 6)
                    _ -> error "pitch_72et: alteration?"
        a' = alteration_72et_monzo n'
        x' = fromIntegral x + (fromIntegral n / 6)
        r = (Pitch_R t' (fromIntegral n' % 12,a') (octave p),fmidi_to_cps x')
        r' = if n > 3
             then pitch_72et (x + 1,n - 6)
             else if n < (-3)
                  then pitch_72et (x - 1,n + 6)
                  else r
    in case a of
         Natural -> r'
         _ -> r

-- | 72-tone equal temperament table equating 'Pitch'' and frequency
-- over range of human hearing, where @A4@ = @440@hz.
--
-- > length tbl_72et == 792
-- > min_max (map (round . snd) tbl_72et) == (16,33167)
tbl_72et :: [(Pitch_R,Double)]
tbl_72et =
    let f n = map pitch_72et (zip (replicate 6 n) [0..5])
    in concatMap f [12 .. 143]

-- | 'nearest_et_table_tone' for 'tbl_72et'.
--
-- > let r = "324.0 E<4 323.3 0.7 3.5"
-- > in unwords (hs_r_pp pitch'_pp 1 (nearest_72et_tone 324))
--
-- > let {f = take 2 . hs_r_pp pitch'_pp 1 . nearest_72et_tone . snd}
-- > in mapM_ (print . unwords . f) tbl_72et
nearest_72et_tone :: Double -> HS_R Pitch_R
nearest_72et_tone = nearest_et_table_tone tbl_72et

-- * Detune

-- | 'Pitch' with 12-ET/24-ET tuning deviation given in 'Cents'.
type Pitch_Detune = (Pitch,Cents)

-- | Extract 'Pitch_Detune' from 'HS_R'.
hsr_to_pitch_detune :: HS_R Pitch -> Pitch_Detune
hsr_to_pitch_detune (_,p,_,_,c) = (p,c)

-- | Nearest 12-ET 'Pitch_Detune' to indicated frequency (hz).
--
-- > nearest_pitch_detune_12et 452.8929841231365
nearest_pitch_detune_12et :: Double -> Pitch_Detune
nearest_pitch_detune_12et = hsr_to_pitch_detune . nearest_12et_tone

-- | Nearest 24-ET 'Pitch_Detune' to indicated frequency (hz).
--
-- > nearest_pitch_detune_24et 452.8929841231365
nearest_pitch_detune_24et :: Double -> Pitch_Detune
nearest_pitch_detune_24et = hsr_to_pitch_detune . nearest_24et_tone

-- | Given /near/ function, /f0/ and ratio derive 'Pitch_Detune'.
ratio_to_pitch_detune :: (Double -> HS_R Pitch) -> OctPC -> Rational -> Pitch_Detune
ratio_to_pitch_detune near_f f0 r =
    let f = octpc_to_cps f0 * realToFrac r
        (_,p,_,_,c) = near_f f
    in (p,c)

-- | Frequency (hz) of 'Pitch_Detune'.
--
-- > pitch_detune_to_cps (octpc_to_pitch pc_spell_ks (4,9),50)
pitch_detune_to_cps :: Floating n => Pitch_Detune -> n
pitch_detune_to_cps (p,d) = cps_shift_cents (pitch_to_cps p) (realToFrac d)

-- | 'ratio_to_pitch_detune' of 'nearest_12et_tone'
ratio_to_pitch_detune_12et :: OctPC -> Rational -> Pitch_Detune
ratio_to_pitch_detune_12et = ratio_to_pitch_detune nearest_12et_tone

-- | 'ratio_to_pitch_detune' of 'nearest_24et_tone'
ratio_to_pitch_detune_24et :: OctPC -> Rational -> Pitch_Detune
ratio_to_pitch_detune_24et = ratio_to_pitch_detune nearest_24et_tone

pitch_detune_in_octave_nearest  :: Pitch -> Pitch_Detune -> Pitch_Detune
pitch_detune_in_octave_nearest p1 (p2,d2) = (pitch_in_octave_nearest p1 p2,d2)

-- | Markdown pretty-printer for 'Pitch_Detune'.
pitch_detune_md :: Pitch_Detune -> String
pitch_detune_md (p,c) = pitch_pp p ++ cents_diff_md (round c :: Integer)

-- | HTML pretty-printer for 'Pitch_Detune'.
pitch_detune_html :: Pitch_Detune -> String
pitch_detune_html (p,c) = pitch_pp p ++ cents_diff_html (round c :: Integer)

-- | No-octave variant of 'pitch_detune_md'.
pitch_class_detune_md :: Pitch_Detune -> String
pitch_class_detune_md (p,c) = pitch_class_pp p ++ cents_diff_md (round c :: Integer)

-- | No-octave variant of 'pitch_detune_html'.
pitch_class_detune_html :: Pitch_Detune -> String
pitch_class_detune_html (p,c) = pitch_class_pp p ++ cents_diff_html (round c :: Integer)
