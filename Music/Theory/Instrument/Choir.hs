module Music.Theory.Instrument.Choir where

import Data.List.Split {- split -}

import qualified Music.Theory.Clef as Clef {- hmt -}
import qualified Music.Theory.List as List {- hmt -}
import qualified Music.Theory.Pitch as Pitch {- hmt -}
import qualified Music.Theory.Pitch.Name as Pitch {- hmt -}

-- | Voice types.
data Voice = Bass | Tenor | Alto | Soprano
           deriving (Eq,Ord,Enum,Bounded,Show)

-- | Single character abbreviation for 'Voice'.
voice_abbrev :: Voice -> Char
voice_abbrev = List.head_err . show

-- | Standard 'Clef' for 'Voice'.
voice_clef :: Integral i => Voice -> Clef.Clef i
voice_clef v =
    case v of
      Bass -> Clef.Clef Clef.Bass 0
      Tenor -> Clef.Clef Clef.Treble (-1)
      Alto -> Clef.Clef Clef.Treble 0
      Soprano -> Clef.Clef Clef.Treble 0

-- | Table giving ranges for 'Voice's.
type Voice_Rng_Tbl = [(Voice,(Pitch.Pitch,Pitch.Pitch))]

-- | More or less standard choir ranges, /inclusive/.
voice_rng_tbl_std :: Voice_Rng_Tbl
voice_rng_tbl_std =
    [(Bass,(Pitch.d2,Pitch.c4))
    ,(Tenor,(Pitch.c3,Pitch.a4))
    ,(Alto,(Pitch.f3,Pitch.f5))
    ,(Soprano,(Pitch.c4,Pitch.a5))]

-- | More conservative ranges, /inclusive/.
voice_rng_tbl_safe :: Voice_Rng_Tbl
voice_rng_tbl_safe =
    [(Bass,(Pitch.g2,Pitch.c4))
    ,(Tenor,(Pitch.c3,Pitch.f4))
    ,(Alto,(Pitch.g3,Pitch.c5))
    ,(Soprano,(Pitch.c4,Pitch.f5))]

-- | Lookup voice range table.
voice_rng :: Voice_Rng_Tbl -> Voice -> (Pitch.Pitch,Pitch.Pitch)
voice_rng tbl v = List.lookup_err v tbl

-- | Lookup 'voice_rng_tbl_std'.
voice_rng_std :: Voice -> (Pitch.Pitch,Pitch.Pitch)
voice_rng_std = voice_rng voice_rng_tbl_std

-- | Lookup 'voice_rng_tbl_safe'.
voice_rng_safe :: Voice -> (Pitch.Pitch,Pitch.Pitch)
voice_rng_safe = voice_rng voice_rng_tbl_safe

-- | Is /p/ '>=' /l/ and '<=' /r/.
in_range_inclusive :: Ord a => a -> (a,a) -> Bool
in_range_inclusive p (l,r) = p >= l && p <= r

{- | Is /p/ in range for /v/, (/std/ & /safe/).

>>> map (in_voice_rng Pitch.c4) [Bass .. Soprano]
[(True,True),(True,True),(True,True),(True,True)]
-}
in_voice_rng :: Pitch.Pitch -> Voice -> (Bool,Bool)
in_voice_rng p v =
    (in_range_inclusive p (voice_rng_std v)
    ,in_range_inclusive p (voice_rng_safe v))

-- | Given /tbl/ list 'Voice's that can sing 'Pitch.Pitch'.
possible_voices :: Voice_Rng_Tbl -> Pitch.Pitch -> [Voice]
possible_voices tbl p =
    let f = in_range_inclusive p . voice_rng tbl
    in filter f [Bass .. Soprano]

-- | /std/ variant.
possible_voices_std :: Pitch.Pitch -> [Voice]
possible_voices_std = possible_voices voice_rng_tbl_std

-- | /safe/ variant.
possible_voices_safe :: Pitch.Pitch -> [Voice]
possible_voices_safe = possible_voices voice_rng_tbl_safe

-- | Enumeration of SATB voices.
satb :: [Voice]
satb = [Soprano,Alto,Tenor,Bass]

-- | Names of 'satb'.
satb_name :: [String]
satb_name = map show satb

-- | 'voice_abbrev' of 'satb' as 'String's.
satb_abbrev :: [String]
satb_abbrev = map (return . voice_abbrev) satb

-- | Voice & part number.
type Part = (Voice,Int)

-- | /k/ part choir, ordered by voice.
ch_satb_seq :: Int -> [Part]
ch_satb_seq k = [(vc,n) | vc <- satb, n <- [1..k]]

{- | 'ch_satb_seq' grouped in parts.

>>> map (map part_nm) (ch_parts 4)
[["S1","S2","S3","S4"],["A1","A2","A3","A4"],["T1","T2","T3","T4"],["B1","B2","B3","B4"]]
-}
ch_parts :: Int -> [[Part]]
ch_parts k = chunksOf k (ch_satb_seq k)

{- | Abreviated name for part.

>>> part_nm (Soprano,1)
"S1"
-}
part_nm :: Part -> String
part_nm (v,n) = voice_abbrev v : show n

{- | /k/ SATB choirs, grouped by choir.

>>> k_ch_groups 2
[[(Soprano,1),(Alto,1),(Tenor,1),(Bass,1)],[(Soprano,2),(Alto,2),(Tenor,2),(Bass,2)]]
-}
k_ch_groups :: Int -> [[Part]]
k_ch_groups k =
    let f n = map (\p -> (p,n)) satb
    in map f [1 .. k]

-- | 'concat' of 'k_ch_groups'.
k_ch_groups' :: Int -> [Part]
k_ch_groups' = concat . k_ch_groups

{- | Two /k/ part SATB choirs in score order.

>>> map part_nm (concat (dbl_ch_parts 4))
["S1","S2","A1","A2","T1","T2","B1","B2","S3","S4","A3","A4","T3","T4","B3","B4"]
-}
dbl_ch_parts :: Int -> [[Part]]
dbl_ch_parts k =
    let v = satb
        f p = map (\n -> (p,n))
        g = zipWith f v . replicate 4
    in concatMap g (chunksOf (k `div` 2) [1 .. k])

-- | 'voice_clef' for 'Part's.
mk_clef_seq :: [Part] -> [Clef.Clef Int]
mk_clef_seq = map (voice_clef . fst)
