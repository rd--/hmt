-- | <http://www.ivan-wyschnegradsky.fr/en/chromatic-drawings/>
module Music.Theory.Wyschnegradsky where

import Data.Char {- base -}
import Data.List {- list -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}

import Music.Theory.List {- hmt -}
import Music.Theory.Pitch {- hmt -}
import Music.Theory.Pitch.Spelling {- hmt -}

-- | In a modulo /m/ system, normalise step increments to be either -1
-- or 1.  Non steps raise an error.
--
-- > map (normalise_step 6) [-5,-1,1,5] == [1,-1,1,-1]
normalise_step :: (Eq n,Num n) => n -> n -> n
normalise_step m n
    | n == 1 = 1
    | n == -1 = -1
    | n == m - 1 = -1
    | n == 1 - m = 1
    | otherwise = error "normalise_step"

-- | Wyschnegradsky writes the direction sign at the end of the number.
--
-- > map parse_num_sign ["2+","4-"] == [2,-4]
parse_num_sign :: (Num n, Read n) => String -> n
parse_num_sign s =
    case separate_last s of
      (n,'+') -> read n
      (n,'-') -> negate (read n)
      _ -> error "parse_num_sign"

-- | Expand a chromatic (step-wise) sequence, sign indicates direction.
--
-- > map vec_expand [2,-4] == [[1,1],[-1,-1,-1,-1]]
vec_expand :: Num n => Int -> [n]
vec_expand n = if n > 0 then replicate n 1 else replicate (abs n) (-1)

-- | Parse the vector notation used in some drawings, a comma
-- separated list of chromatic sequences.
--
-- > parse_vec Nothing 0 "4-,4+,4-,4+,4-,4+,4-,4+,4-"
-- > parse_vec Nothing 0 "2+,2-,2+,2-,2+,2-,2+,2-,2+,18+"
parse_vec :: Num n => Maybe Int -> n -> String -> [n]
parse_vec n m =
    let f = case n of
              Just i -> dx_d m . take i . cycle
              Nothing -> dx_d m
    in dropRight 1 . f . concatMap (vec_expand . parse_num_sign) . splitOn ","

-- | Modulo addition.
add_m :: Integral a => a -> a -> a -> a
add_m n p q = (p + q) `mod` n

-- | Parse hex colour string, as standard in HTML5.
--
-- > parse_hex_clr "#e14630" == (225,70,48)
parse_hex_clr :: (Read n,Num n) => [Char] -> (n,n,n)
parse_hex_clr clr =
    let f p q = read ("0x" ++ [p,q])
    in case clr of
         ['#',p,q,r,s,t,u] -> (f p q,f r s,f t u)
         _ -> error "parse_hex"

-- | Normalise colour by dividing each component by /m/.
--
-- > clr_normalise 255 (parse_hex_clr "#ff0066") == (1,0,0.4)
clr_normalise :: (Real r,Fractional f) => f -> (r,r,r) -> (f,f,f)
clr_normalise m (r,g,b) = let f x = realToFrac x / m in (f r,f g,f b)

-- | Sequences are either in 'Radial' or 'Circumferential' order.
data Seq a = Radial [a] | Circumferential [a]

-- | Group sequence into normal (ie. 'Circumferential') order given
-- drawing dimensions.
seq_group :: Int -> Int -> Seq a -> [[a]]
seq_group c_div r_div s =
    case s of
      Circumferential c -> chunksOf c_div c
      Radial r -> transpose (chunksOf r_div r)

-- | Printer for pitch-class segments.
iw_pc_pp :: Integral n => String -> [[n]] -> IO ()
iw_pc_pp sep =
    let f = pitch_class_pp' False . octpc_to_pitch pc_spell_ks . (,) 4
    in putStrLn . intercalate sep . map (unwords . map f)

-- * U3

-- | Index to colour name abbreviation.
--
-- > map u3_ix_ch [0..5] == "ROYGBV"
u3_ix_ch :: Integral i => i -> Char
u3_ix_ch = genericIndex "ROYGBV" . (`mod` 6)

-- | Inverse of 'u3_ix_ch'.
--
-- > map u3_ch_ix "ROYGBV" == [0..5]
u3_ch_ix :: Char -> Int
u3_ch_ix = fromMaybe (error "u3_ch_ix") . flip elemIndex "ROYGBV"

-- | Drawing definition, as written by Wyschnegradsky.
--
-- > mapM_ (\(c,r) -> putStrLn (unlines ["C: " ++ c,"R: " ++ r])) u3_vec_text_iw
u3_vec_text_iw :: [(String, String)]
u3_vec_text_iw =
    [("4+,4-,4+,4-,2+"
     ,"4-,4+,4-,4+,4-,4+,4-,4+,4-")
    ,("9+,2+,2-,2+,2-,2+"
     ,"2+,2-,2+,2-,2+,2-,2+,2-,2+,18+")
    ,("12-,12+,12-"
     ,"18+,18-")
    ,("3+,3-,3+,3-,3+,3-"
     ,"18+,18-")
    ,("9+,9-"
     ,"3+,3-,3+,3-,3+,3-,3+,3-,3+,3-,3+,3-")
    ,("2+,2-,2+,2-,2+,2-"
     ,"6-,6+,6-,6+,6-,6+")
    ,("2+,2-,2+,2-,2+,2-"
     ,"6+,6-,6+,6-,6+,6-")
    ,("6+,6-"
     ,"2+,2-,2+,2-,2+,2-,2+,2-,2+,2-,2+,2-,2+,2-,2+,2-,2+,2-")]

-- | Re-written for local parser and to correct ambiguities and errors
-- (to align with actual drawing).
--
-- > let f = parse_vec Nothing 0 in map (\(p,q) -> (f p,f q)) u3_vec_text_rw
--
-- > let f (c,r) = putStrLn (unlines ["C: " ++ c,"R: " ++ r])
-- > in mapM_ f (interleave u3_vec_text_iw u3_vec_text_rw)
u3_vec_text_rw :: [(String, String)]
u3_vec_text_rw =
    [("4+,3-,5+,3-,3+"
     ,"4-,3+,5-,3+,5-,3+,5-,3+,5-") -- 1
    ,("9+,2+,1-,3+,1-,2+"
     ,"2+,1-,3+,1-,3+,1-,3+,1-,3+,1-,3+,1-,3+,1-,3+,1-,3+,2-") -- 2
    ,("12-,12+,12-"
     ,"18+,18-")
    ,("3+,2-,4+,2-,4+,3-"
     ,"18+,18-")
    ,("9+,9-"
     ,"3+,2-,4+,1-,1+,1-,3+,1-,1+,1-,3+,2-,4+,1-,1+,1-,3+,1-,1+,1-") -- 5
    ,("2+,1-,3+,1-,3+,2-"
     ,"6-,6+,6-,6+,6-,6+") -- 6
    ,("2+,1-,3+,1-,3+,2-"
     ,"6+,6-,6+,6-,6+,6-") -- 7
    ,("6+,6-"
     ,"2+,1-,3+,1-,3+,1-,3+,1-,3+,1-,3+,1-,3+,1-,3+,1-,3+,2-")] -- 8

-- | Parse of 'u3_vec_text_rw'.
--
-- > let {(c,r) = u3_vec_ix ; c' = map length c}
-- > in (length c,c',sum c',length r,map length r)
u3_vec_ix :: Num n => ([[n]],[[n]])
u3_vec_ix =
    let f (p,q) = [parse_vec Nothing 0 p,parse_vec Nothing 0 q]
        [c,r] = transpose (map f u3_vec_text_rw)
    in (c,r)

-- | Radial indices (ie. each /ray/ as an index sequence).
--
-- > putStrLn $ unlines $ map (map u3_ix_ch) u3_ix_radial
u3_ix_radial :: Integral n => [[n]]
u3_ix_radial =
    let (c,r) = u3_vec_ix
        r' = zipWith replicate (map length c) r
    in zipWith (\p q -> map (add_m 6 p) q) (concat c) (concat r')

-- | Colour names in index sequence.
u3_clr_nm :: [String]
u3_clr_nm = words "red orange yellow green blue violet"

-- | Colour values (hex strings) in index sequence.
u3_clr_hex :: [String]
u3_clr_hex = words "#e14630 #e06e30 #e2c48e #498b43 #2a5a64 #cb7b74"

-- | RGB form of 'u3_clr_hex'.
u3_clr_rgb :: (Read n,Fractional n) => [(n,n,n)]
u3_clr_rgb = map (clr_normalise 256 . parse_hex_clr) u3_clr_hex

-- | Notated radial color sequence, transcribed from drawing.
--
-- > map (\(n,c) -> let v = u3_ch_seq_to_vec c in (n,sum v,v)) u3_radial_ch
u3_radial_ch :: [(Int,[Char])]
u3_radial_ch =
    [(1,"RVBGY GBV BGYOR OYG YORVB VRO RVBGY GBVBGYO")
    ,(5,"ROYG YO YGBV BV BVRO RO ROYG YO YGBV BV BVR OR O")]

-- | Notated circumferenctial color sequence, transcribed from drawing.
--
-- > map (\(n,c) -> (n,u3_ch_seq_to_vec c)) u3_circ_ch
u3_circ_ch :: [(Int,[Char])]
u3_circ_ch =
    [(6,"ROYOYGBGBVRV")
    ,(7,"ROYOYGBGBVRV")
    ,(8,"ROYGBVRVBGYO")]

-- | Translate notated sequence to "re-written" vector notation.
u3_ch_seq_to_vec :: [Char] -> [Int]
u3_ch_seq_to_vec =
    map length .
    group .
    map (normalise_step 6) .
    d_dx .
    map u3_ch_ix .
    filter (not . isSpace)

-- * DC9

-- | Circumference pitch classes, C = 0.
--
-- > let c' = map length dc9_circ in (sum c',c') == (72,[5,6,7,2,3,4,4,3,2,7,7,4,4,3,2,2,3,4])
--
-- > iw_pc_pp " | " dc9_circ
dc9_circ :: Num n => [[n]]
dc9_circ =
    [[6,5,4,3,2]
    ,[3,2,1,0,11,10]
    ,[11,10,9,8,7,6,5]
    ,[6,5]
    ,[6,5,4]
    ,[5,4,3,2]
    ,[3,2,1,0]
    ,[1,0,11]
    ,[0,11]
    ,[0,1,2,3,4,5,6]
    ,[5,6,7,8,9,10,9]
    ,[10,11,0,1]
    ,[0,1,2,3]
    ,[2,3,4]
    ,[3,4]
    ,[3,4]
    ,[3,4,5]
    ,[4,5,6,7]]

-- | Rayon pitch classes, C = 0.
--
-- > length dc9_rad == 18
-- > putStrLn $ unwords $ map f dc9_rad
dc9_rad :: Num n => [n]
dc9_rad = [0,10,8,6,4,2,0,10,8,6,4,2,0,10,8,6,4,2]

-- | Radial indices.
--
-- > map length dc9_ix == replicate 72 18
dc9_ix :: Integral n => [[n]]
dc9_ix = map (\n -> map (add_m 12 n) dc9_rad) (concat dc9_circ)

-- | Approximate colours, hex strings.
dc9_clr_hex :: [String]
dc9_clr_hex =
    let c = ["#e96d61","#e6572b"
            ,"#e07122","#e39e36"
            ,"#e8b623","#e5c928"
            ,"#c2ba3d","#a2a367"
            ,"#537a77","#203342"
            ,"#84525e","#bc6460"]
        n = interleave [6,4,2,0,10,8] [5,3,1,11,9,7]
    in map snd (sort (zip n c))

-- | RGB form of colours.
dc9_clr_rgb :: (Read n,Fractional n) => [(n,n,n)]
dc9_clr_rgb = map (clr_normalise 255 . parse_hex_clr) dc9_clr_hex

-- * U11

-- > 18 * 4 == 72
-- > let c' = map length u11_circ in (sum c',length c',c')
--
-- > iw_pc_pp "\n- " u11_circ
u11_circ :: Num n => [[n]]
u11_circ =
    [[7,8,9,10,11,0,1,2,3]
    ,[10,11,0,1,2,3,4,5,6]
    ,[0,1,2,3,4,5]
    ,[0,1,2]
    ,[10,11]
    ,[6,7]
    ,[2]
    ,[9]
    ,[4]
    ,[11]
    ,[6,7]
    ,[2]
    ,[9]
    ,[2]
    ,[11]
    ,[6,7]
    ,[2,3]
    ,[10,11,0]
    ,[7,8,9,10,11,0]
    ,[7,8,9,10,11,0,1,2,3]
    ,[10,11,0,1,2,3,4,5,6]]

-- > iw_pc_pp "|" [u11_gen_seq 7 18 [5]]
u11_gen_seq :: Integral i => i -> Int -> [i] -> [i]
u11_gen_seq z n = map (`mod` 12) . take n . dx_d z . cycle

u11_seq_rule :: Integral i => Maybe Int -> [i]
u11_seq_rule n = u11_gen_seq 0 18 (maybe [-1] (\x -> replicate x (-1) ++ [5]) n)

-- > ull_rad_text == "012588---------885210"
ull_rad_text :: [Char]
ull_rad_text =
    let x = "012588----"
        y = "-"
    in x ++ y ++ reverse x

-- > iw_pc_pp "\n- " u11_rad
u11_rad :: Integral n => [[n]]
u11_rad =
    let f c = if c == '-' then Nothing else Just (read [c])
    in map (u11_seq_rule . f) ull_rad_text

u11_clr_hex :: [String]
u11_clr_hex =
    let c = ["#dbb56a","#ffb05c","#ea7c3f","#f93829","#ee6054","#d18d9c"
            ,"#a94c79","#215272","#628b7d","#9dbc90","#ecdfaa","#fbeaa5"]
        n = reverse ([4..11] ++ [0..3])
    in map snd (sort (zip n c))

u11_clr_rgb :: (Read n,Fractional n) => [(n,n,n)]
u11_clr_rgb = map (clr_normalise 256 . parse_hex_clr) u11_clr_hex
