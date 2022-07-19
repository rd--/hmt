-- | Square arrays, where the number of rows and columns are equal.
module Music.Theory.Array.Square where

import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.Map as Map {- containers -}
import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.Array as T {- hmt-base -}
import qualified Music.Theory.Array.Text as T {- hmt-base -}
import qualified Music.Theory.List as T {- hmt-base -}

import qualified Music.Theory.Math.Oeis as T {- hmt -}

-- | Square as list of lists.
type Square t = [[t]]

-- | Squares are functors
sq_map :: (t -> t) -> Square t -> Square t
sq_map f = map (map f)

-- | 'sq_map' of '*' /n/
sq_scale :: Num t => t -> Square t -> Square t
sq_scale n = sq_map (* n)

-- | /f/ pointwise at two squares (of equal size, un-checked)
sq_zip :: (t -> t -> t) -> Square t -> Square t -> Square t
sq_zip f = zipWith (zipWith f)

-- | 'sq_zip' of '*'
sq_mul :: Num t => Square t -> Square t -> Square t
sq_mul = sq_zip (*)

-- | 'sq_zip' of '+'
sq_add :: Num t => Square t -> Square t -> Square t
sq_add = sq_zip (+)

-- | 'foldl1' of 'sq_add'
sq_sum :: Num t => [Square t] -> Square t
sq_sum = foldl1 sq_add

-- | Predicate to determine if 'Square' is actually square.
sq_is_square :: Square t -> Bool
sq_is_square sq = nub (map length sq) == [length sq]

-- | Square as row order list
type Square_Linear t = [t]

-- | Given degree of square, form 'Square' from 'Square_Linear'.
sq_from_list :: Int -> Square_Linear t -> Square t
sq_from_list = Split.chunksOf

-- | True if list can form a square, ie. if 'length' is a square.
--
-- > sq_is_linear_square T.a126710 == True
sq_is_linear_square :: Square_Linear t -> Bool
sq_is_linear_square l = length l `T.elem_ordered` T.a000290

-- | Calculate degree of linear square, ie. square root of 'length'.
--
-- > sq_linear_degree T.a126710 == 4
sq_linear_degree :: Square_Linear t -> Int
sq_linear_degree =
    fromMaybe (error "sq_linear_degree") .
    flip T.elemIndex_ordered T.a000290 .
    length

-- | Type specialised 'transpose'
sq_transpose :: Square t -> Square t
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
sq_diagonals_ul_lr :: Square t -> Square t
sq_diagonals_ul_lr = sq_transpose . zipWith T.rotate_left [0..]

-- | Full lower-left (ll) to upper-right (ur) diagonals of a square.
sq_diagonals_ll_ur :: Square t -> Square t
sq_diagonals_ll_ur = sq_diagonals_ul_lr . reverse

-- | Inverse of 'diagonals_ul_lr'
sq_undiagonals_ul_lr :: Square t -> Square t
sq_undiagonals_ul_lr = zipWith T.rotate_right [0..] . sq_transpose

-- | Inverse of 'diagonals_ll_ur'
sq_undiagonals_ll_ur :: Square t -> Square t
sq_undiagonals_ll_ur = reverse . sq_undiagonals_ul_lr

-- | Main diagonal (upper-left -> lower-right)
sq_diagonal_ul_lr :: Square t -> [t]
sq_diagonal_ul_lr sq = zipWith (!!) sq [0 ..]

-- | Main diagonal (lower-left -> upper-right)
sq_diagonal_ll_ur :: Square t -> [t]
sq_diagonal_ll_ur = sq_diagonal_ul_lr . reverse

{- | Horizontal reflection (ie. map reverse).

> sq = sq_from_list 4 T.a126710
> sq_wr $ sq
> sq_wr $ sq_h_reflection sq

-}
sq_h_reflection :: Square t -> Square t
sq_h_reflection = map reverse

-- | An n×n square is /normal/ if it has the elements (1 .. n×n).
sq_is_normal :: Integral n => Square n -> Bool
sq_is_normal sq =
  let n = genericLength sq
  in sort (concat sq) == [1 .. n * n]

-- | Sums of (rows, columns, left-right-diagonals, right-left-diagonals)
sq_sums :: Num n => Square n -> ([n],[n],[n],[n])
sq_sums sq =
  (map sum sq
  ,map sum (sq_transpose sq)
  ,map sum (sq_diagonals_ul_lr sq)
  ,map sum (sq_diagonals_ll_ur sq))

-- * PP

sq_opt :: T.Text_Table_Opt
sq_opt = (False,True,False," ",False)

sq_pp :: Show t => Square t -> String
sq_pp = unlines . T.table_pp_show sq_opt

sq_wr :: Show t => Square t -> IO ()
sq_wr = putStrLn . ('\n' :) . sq_pp

sq_pp_m :: Show t => String -> Square (Maybe t) -> String
sq_pp_m e = unlines . T.table_pp sq_opt . map (map (maybe e (T.pad_left '·' 2 . show)))

sq_wr_m :: Show t => String -> Square (Maybe t) -> IO ()
sq_wr_m e = putStrLn . sq_pp_m e

-- * Square Map

-- | (row,column) index.
type Square_Ix = T.Ix Int

-- | Map from Square_Ix to value.
type Square_Map t = Map.Map Square_Ix t

-- | 'Square' to 'Square_Map'.
sq_to_map :: Square t -> Square_Map t
sq_to_map =
    let f r = zipWith (\c e -> ((r,c),e)) [0..]
    in Map.fromList . concat . zipWith f [0..]

-- | Alias for 'Map.!'
sqm_ix :: Square_Map t -> Square_Ix -> t
sqm_ix = (Map.!)

-- | 'map' of 'sqm_ix'.
sqm_ix_seq :: Square_Map t -> [Square_Ix] -> [t]
sqm_ix_seq m = map (sqm_ix m)

-- | Make a 'Square' of dimension /dm/ that has elements from /m/ at
-- indicated indices, else 'Nothing'.
sqm_to_partial_sq :: Int -> Square_Map t -> [Square_Ix] -> Square (Maybe t)
sqm_to_partial_sq dm m ix_set =
    let f i = if i `elem` ix_set then Just (m Map.! i) else Nothing
    in Split.chunksOf dm (map f (T.matrix_indices (dm,dm)))

-- * TRS SEQ

sq_trs_op :: [(String,Square t -> Square t)]
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

sq_trs_seq :: Square t -> [(String,Square t)]
sq_trs_seq sq = map (\(nm,fn) -> (nm,fn sq)) sq_trs_op

