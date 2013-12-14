-- | Equal temperament tuning tables.
module Music.Theory.Tuning.ET where

import Text.Printf {- base -}

import Music.Theory.List {- hmt -}
import Music.Theory.Pitch {- hmt -}
import Music.Theory.Pitch.Spelling {- hmt -}
import Music.Theory.Tuning {- hmt -}

-- | 'octpc_to_pitch' and 'octpc_to_cps'.
octpc_to_pitch_cps :: (Floating n) => OctPC -> (Pitch,n)
octpc_to_pitch_cps x = (octpc_to_pitch pc_spell_ks x,octpc_to_cps x)

-- | 12-tone equal temperament table equating 'Pitch' and frequency
-- over range of human hearing, where @A4@ = @440@hz.
--
-- > length tbl_12et == 132
-- > min_max (map (round . snd) tbl_12et) == (16,31609)
tbl_12et :: [(Pitch,Double)]
tbl_12et =
    let z = [(o,pc) | o <- [0..10], pc <- [0..11]]
    in map octpc_to_pitch_cps z

-- | 24-tone equal temperament variant of 'tbl_12et'.
--
-- > length tbl_24et == 264
-- > min_max (map (round . snd) tbl_24et) == (16,32535)
tbl_24et :: [(Pitch, Double)]
tbl_24et =
    let f x = let p = fmidi_to_pitch pc_spell_ks x
                  p' = pitch_rewrite_threequarter_alteration p
              in (p',fmidi_to_cps x)
    in map f [12,12.5 .. 143.5]

-- | Given an @ET@ table (or like) find bounds of frequency.
--
-- > let r = Just (at_pair octpc_to_pitch_cps ((3,11),(4,0)))
-- > in bounds_et_table tbl_12et 256 == r
bounds_et_table :: Ord s => [(t,s)] -> s -> Maybe ((t,s),(t,s))
bounds_et_table tbl =
    let f (_,p) = compare p
    in find_bounds f (adj2 1 tbl)

-- | 'bounds_et_table' of 'tbl_12et'.
--
-- > map bounds_12et_tone (hsn 17 55)
bounds_12et_tone :: Double -> Maybe ((Pitch,Double),(Pitch,Double))
bounds_12et_tone = bounds_et_table tbl_12et

-- | Tuple indicating nearest 'Pitch' to /frequency/ with @ET@
-- frequency, and deviation in hertz and 'Cents'.
type HS_R = (Double,Pitch,Double,Double,Cents)

-- | /n/-decimal places.
--
-- > ndp 3 (1/3) == "0.333"
ndp :: Int -> Double -> String
ndp = printf "%.*f"

-- | Pretty print 'HS_R'.
hs_r_pp :: Int -> HS_R -> [String]
hs_r_pp n (f,p,pf,fd,c) =
    let dp = ndp n
    in [dp f
       ,pitch_pp p
       ,dp pf
       ,dp fd
       ,dp c]

-- | Form 'HS_R' for /frequency/ by consulting table.
--
-- > let {f = 256
-- >     ;f' = octpc_to_cps (4,0)
-- >     ;r = (f,Pitch C Natural 4,f',f-f',to_cents (f/f'))}
-- > in nearest_et_table_tone tbl_12et 256 == r
nearest_et_table_tone :: [(Pitch,Double)] -> Double -> HS_R
nearest_et_table_tone tbl f =
    case bounds_et_table tbl f of
      Nothing -> undefined
      Just ((lp,lf),(rp,rf)) ->
          let ld = f - lf
              rd = f - rf
          in if abs ld < abs rd
             then (f,lp,lf,ld,to_cents (f/lf))
             else (f,rp,rf,rd,to_cents (f/rf))

-- | 'nearest_et_table_tone' for 'tbl_12et'.
nearest_12et_tone :: Double -> HS_R
nearest_12et_tone = nearest_et_table_tone tbl_12et

-- | 'nearest_et_table_tone' for 'tbl_24et'.
--
-- > unwords (hs_r_pp 1 (nearest_24et_tone 55)) == "55.0 A1 55.0 0.0 0.0"
nearest_24et_tone :: Double -> HS_R
nearest_24et_tone = nearest_et_table_tone tbl_24et
