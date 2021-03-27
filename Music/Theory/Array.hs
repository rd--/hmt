-- | Array & table functions
module Music.Theory.Array where

import Data.List {- base -}
import qualified Data.Array as A {- array -}

import qualified Music.Theory.List as T {- hmt -}

-- * Association List (List Array)

-- | 'T.minmax' of /k/.
larray_bounds :: Ord k => [(k,v)] -> (k,k)
larray_bounds = T.minmax . map fst

-- | 'A.array' of association list.
larray :: A.Ix k => [(k,v)] -> A.Array k v
larray a = A.array (larray_bounds a) a

-- * List Table

-- | Plain list representation of a two-dimensional table of /a/ in
-- row-order.  Tables are regular, ie. all rows have equal numbers of
-- columns.
type Table a = [[a]]

-- | Table row count.
tbl_rows :: Table t -> Int
tbl_rows = length

-- | Table column count, assumes table is regular.
tbl_columns :: Table t -> Int
tbl_columns tbl =
  case tbl of
    [] -> 0
    r0:_ -> length r0

-- | Determine is table is regular, ie. all rows have the same number of columns.
--
-- > tbl_is_regular [[0..3],[4..7],[8..11]] == True
tbl_is_regular :: Table t -> Bool
tbl_is_regular = (== 1) . length . nub . map length

-- | Map /f/ at table, padding short rows with /k/.
tbl_make_regular :: (t -> u,u) -> Table t -> Table u
tbl_make_regular (f,k) tbl =
    let z = maximum (map length tbl)
    in map (T.pad_right k z . map f) tbl

-- | Append a sequence of /nil/ (or default) values to each row of /tbl/
-- so to make it regular (ie. all rows of equal length).
tbl_make_regular_nil :: t -> Table t -> Table t
tbl_make_regular_nil k = tbl_make_regular (id,k)

-- * Matrix Indices

-- | Matrix dimensions are written (rows,columns).
type Dimensions i = (i,i)

-- | Matrix indices are written (row,column) & are here _zero_ indexed.
type Ix i = (i,i)

-- | Translate 'Ix' by row and column delta.
--
-- > ix_translate (1,2) (3,4) == (4,6)
ix_translate :: Num t => (t,t) -> Ix t -> Ix t
ix_translate (dr,dc) (r,c) = (r + dr,c + dc)

-- | Modulo 'Ix' by 'Dimensions'.
--
-- > ix_modulo (4,4) (3,7) == (3,3)
ix_modulo :: Integral t => Dimensions t -> Ix t -> Ix t
ix_modulo (nr,nc) (r,c) = (r `mod` nr,c `mod` nc)

-- | Given number of columns and row index, list row indices.
--
-- > row_indices 3 1 == [(1,0),(1,1),(1,2)]
row_indices :: (Enum t, Num t) => t -> t -> [Ix t]
row_indices nc r = map (\c -> (r,c)) [0 .. nc - 1]

-- | Given number of rows and column index, list column indices.
--
-- > column_indices 3 1 == [(0,1),(1,1),(2,1)]
column_indices :: (Enum t, Num t) => t -> t -> [Ix t]
column_indices nr c = map (\r -> (r,c)) [0 .. nr - 1]

-- | All zero-indexed matrix indices, in row order.  This is the order
-- given by 'sort'.
--
-- > matrix_indices (2,3) == [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
-- > sort (matrix_indices (2,3)) == matrix_indices (2,3)
matrix_indices :: (Enum t, Num t) => Dimensions t -> [Ix t]
matrix_indices (nr,nc) = concatMap (row_indices nc) [0 .. nr - 1 ]

-- | Corner indices of given 'Dimensions', in row order.
--
-- > matrix_corner_indices (2,3) == [(0,0),(0,2),(1,0),(1,2)]
matrix_corner_indices :: Num t => Dimensions t -> [Ix t]
matrix_corner_indices (nr,nc) = [(0,0),(0,nc - 1),(nr - 1,0),(nr - 1,nc - 1)]

-- | Parallelogram corner indices, given as rectangular 'Dimensions' with an
-- offset for the lower indices.
--
-- > parallelogram_corner_indices ((2,3),2) == [(0,0),(0,2),(1,2),(1,4)]
parallelogram_corner_indices :: Num t => (Dimensions t,t) -> [Ix t]
parallelogram_corner_indices ((nr,nc),o) = [(0,0),(0,nc - 1),(nr - 1,o),(nr - 1,nc + o - 1)]

-- | Apply 'ix_modulo' and 'ix_translate' for all 'matrix_indices',
-- ie. all translations of a 'shape' in row order.  The resulting 'Ix'
-- sets are not sorted and may have duplicates.
--
-- > concat (all_ix_translations (2,3) [(0,0)]) == matrix_indices (2,3)
all_ix_translations :: Integral t => Dimensions t -> [Ix t] -> [[Ix t]]
all_ix_translations dm ix =
    let f z = ix_modulo dm . ix_translate z
    in map (\dx -> map (f dx) ix) (matrix_indices dm)

-- | Sort sets into row order and remove duplicates.
all_ix_translations_uniq :: Integral t => Dimensions t -> [Ix t] -> [[Ix t]]
all_ix_translations_uniq dm = nub . map sort . all_ix_translations dm
