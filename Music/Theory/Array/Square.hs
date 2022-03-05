-- | Square arrays, where the number of rows and columns are equal.
module Music.Theory.Array.Square where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.Map as Map {- containers -}
import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.Array as T {- hmt-base -}
import qualified Music.Theory.Array.Text as T {- hmt-base -}
import qualified Music.Theory.List as T {- hmt-base -}

import qualified Music.Theory.Math.OEIS as T {- hmt -}

-- | Square as list of lists.
type SQ t = [[t]]

-- | Squares are functors
sq_map :: (t -> t) -> SQ t -> SQ t
sq_map f = map (map f)

-- | 'sq_map' of '*' /n/
sq_scale :: Num t => t -> SQ t -> SQ t
sq_scale n = sq_map (* n)

-- | /f/ pointwise at two squares (of equal size, un-checked)
sq_zip :: (t -> t -> t) -> SQ t -> SQ t -> SQ t
sq_zip f = zipWith (zipWith f)

-- | 'sq_zip' of '*'
sq_mul :: Num t => SQ t -> SQ t -> SQ t
sq_mul = sq_zip (*)

-- | 'sq_zip' of '+'
sq_add :: Num t => SQ t -> SQ t -> SQ t
sq_add = sq_zip (+)

-- | 'foldl1' of 'sq_add'
sq_sum :: Num t => [SQ t] -> SQ t
sq_sum = foldl1 sq_add

-- | Predicate to determine if 'SQ' is actually square.
sq_is_square :: SQ t -> Bool
sq_is_square sq = nub (map length sq) == [length sq]

-- | Square as row order list
type SQ_Linear t = [t]

-- | Given degree of square, form 'SQ' from 'SQ_Linear'.
sq_from_list :: Int -> SQ_Linear t -> SQ t
sq_from_list = Split.chunksOf

-- | True if list can form a square, ie. if 'length' is a square.
--
-- > sq_is_linear_square T.a126710 == True
sq_is_linear_square :: SQ_Linear t -> Bool
sq_is_linear_square l = length l `T.elem_ordered` T.a000290

-- | Calculate degree of linear square, ie. square root of 'length'.
--
-- > sq_linear_degree T.a126710 == 4
sq_linear_degree :: SQ_Linear t -> Int
sq_linear_degree =
    fromMaybe (error "sq_linear_degree") .
    flip T.elemIndex_ordered T.a000290 .
    length

-- | Type specialised 'transpose'
sq_transpose :: SQ t -> SQ t
sq_transpose = transpose

{- | Full upper-left (ul) to lower-right (lr) diagonals of a square.

> sq = sq_from_list 4 T.a126710
> sq_wr $ sq
> sq_wr $ sq_diagonals_ul_lr sq
> sq_wr $ sq_diagonals_ll_ur sq
> sq_undiagonals_ul_lr (sq_diagonals_ul_lr sq) == sq
> sq_undiagonals_ll_ur (sq_diagonals_ll_ur sq) == sq

> sq_diagonal_ul_lr sq == sq_diagonals_ul_lr sq !! 0
> sq_diagonal_ll_ur sq == sq_diagonals_ll_ur sq !! 0

-}
sq_diagonals_ul_lr :: SQ t -> SQ t
sq_diagonals_ul_lr = sq_transpose . zipWith T.rotate_left [0..]

-- | Full lower-left (ll) to upper-right (ur) diagonals of a square.
sq_diagonals_ll_ur :: SQ t -> SQ t
sq_diagonals_ll_ur = sq_diagonals_ul_lr . reverse

-- | Inverse of 'diagonals_ul_lr'
sq_undiagonals_ul_lr :: SQ t -> SQ t
sq_undiagonals_ul_lr = zipWith T.rotate_right [0..] . sq_transpose

-- | Inverse of 'diagonals_ll_ur'
sq_undiagonals_ll_ur :: SQ t -> SQ t
sq_undiagonals_ll_ur = reverse . sq_undiagonals_ul_lr

-- | Main diagonal (upper-left -> lower-right)
sq_diagonal_ul_lr :: SQ t -> [t]
sq_diagonal_ul_lr sq = zipWith (!!) sq [0 ..]

-- | Main diagonal (lower-left -> upper-right)
sq_diagonal_ll_ur :: SQ t -> [t]
sq_diagonal_ll_ur = sq_diagonal_ul_lr . reverse

{- | Horizontal reflection (ie. map reverse).

> sq = sq_from_list 4 T.a126710
> sq_wr $ sq
> sq_wr $ sq_h_reflection sq

-}
sq_h_reflection :: SQ t -> SQ t
sq_h_reflection = map reverse

-- | An n×n square is /normal/ if it has the elements (1 .. n×n).
sq_is_normal :: Integral n => SQ n -> Bool
sq_is_normal sq =
  let n = genericLength sq
  in sort (concat sq) == [1 .. n * n]

-- | Sums of (rows, columns, left-right-diagonals, right-left-diagonals)
sq_sums :: Num n => SQ n -> ([n],[n],[n],[n])
sq_sums sq =
  (map sum sq
  ,map sum (sq_transpose sq)
  ,map sum (sq_diagonals_ul_lr sq)
  ,map sum (sq_diagonals_ll_ur sq))

-- * PP

sq_opt :: T.Text_Table_Opt
sq_opt = (False,True,False," ",False)

sq_pp :: Show t => SQ t -> String
sq_pp = unlines . T.table_pp_show sq_opt

sq_wr :: Show t => SQ t -> IO ()
sq_wr = putStrLn . ('\n' :) . sq_pp

sq_pp_m :: Show t => String -> SQ (Maybe t) -> String
sq_pp_m e = unlines . T.table_pp sq_opt . map (map (maybe e (T.pad_left '·' 2 . show)))

sq_wr_m :: Show t => String -> SQ (Maybe t) -> IO ()
sq_wr_m e = putStrLn . sq_pp_m e

-- * SQ Map

-- | (row,column) index.
type SQ_Ix = T.Ix Int

-- | Map from SQ_Ix to value.
type SQ_Map t = Map.Map SQ_Ix t

-- | 'SQ' to 'SQ_Map'.
sq_to_map :: SQ t -> SQ_Map t
sq_to_map =
    let f r = zipWith (\c e -> ((r,c),e)) [0..]
    in Map.fromList . concat . zipWith f [0..]

-- | Alias for 'Map.!'
sqm_ix :: SQ_Map t -> SQ_Ix -> t
sqm_ix = (Map.!)

-- | 'map' of 'sqm_ix'.
sqm_ix_seq :: SQ_Map t -> [SQ_Ix] -> [t]
sqm_ix_seq m = map (sqm_ix m)

-- | Make a 'SQ' of dimension /dm/ that has elements from /m/ at
-- indicated indices, else 'Nothing'.
sqm_to_partial_sq :: Int -> SQ_Map t -> [SQ_Ix] -> SQ (Maybe t)
sqm_to_partial_sq dm m ix_set =
    let f i = if i `elem` ix_set then Just (m Map.! i) else Nothing
    in Split.chunksOf dm (map f (T.matrix_indices (dm,dm)))

-- * TRS SEQ

sq_trs_op :: [(String,SQ t -> SQ t)]
sq_trs_op =
    [("≡",id)
    ,("←",sq_h_reflection)
    ,("↓",sq_transpose)
    ,("(← · ↓)",sq_h_reflection . sq_transpose)
    ,("(↓ · ← · ↓)",sq_transpose . sq_h_reflection . sq_transpose)
    ,("(↓ · ←)",sq_transpose . sq_h_reflection)
    ,("(← · ↓ · ←)",sq_h_reflection . sq_transpose . sq_h_reflection)
    ,("↘",sq_diagonals_ul_lr)
    ,("↙ = (↘ · ←)",sq_diagonals_ul_lr . sq_h_reflection)
    ,("↗ = (← · ↙)",sq_h_reflection . sq_diagonals_ul_lr . sq_h_reflection)
    ,("↖ = (← · ↘)",sq_h_reflection . sq_diagonals_ul_lr)
    ]

sq_trs_seq :: SQ t -> [(String,SQ t)]
sq_trs_seq sq = map (\(nm,fn) -> (nm,fn sq)) sq_trs_op

