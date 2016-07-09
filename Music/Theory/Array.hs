module Music.Theory.Array where

import qualified Data.Array as A {- array -}

import qualified Music.Theory.List as T {- hmt -}

-- * Association List (List Array)

larray_bounds :: Ord k => [(k,v)] -> (k,k)
larray_bounds = T.minmax . map fst

larray :: A.Ix k => [(k,v)] -> A.Array k v
larray a = A.array (larray_bounds a) a

-- * List Table

-- | Append a sequence of /nil/ (or default) values to each row of /tbl/
-- so to make it regular (ie. all rows of equal length).
make_regular :: t -> [[t]] -> [[t]]
make_regular k tbl =
    let z = maximum (map length tbl)
    in map (T.pad_right k z) tbl

-- * Matrix Indices

-- | Matrix dimensions are written (rows,columns).
type Dimensions i = (i,i)

-- | Matrix indices are written (row,column)
type Ix i = (i,i)

-- | Translate Ix by row and column delta.
ix_translate :: Num t => (t,t) -> Ix t -> Ix t
ix_translate (dr,dc) (r,c) = (r + dr,c + dc)

-- | Modulo Ix by dimensions.
ix_modulo :: Integral t => (t,t) -> Ix t -> Ix t
ix_modulo (nr,nc) (r,c) = (r `mod` nr,c `mod` nc)

-- | All zero indexed matrix indices, in row order.
--
-- > matrix_indices (2,3) == [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
matrix_indices :: (Enum t, Num t) => Dimensions t -> [Ix t]
matrix_indices (nr,nc) =
    let f r = map (\c -> (r,c)) [0 .. nc - 1]
    in concatMap f [0 .. nr - 1 ]

-- | Rectangle corner indices in row order.
--
-- > rectangle_corner_indices (2,3) == [(0,0),(0,2),(1,0),(1,2)]
rectangle_corner_indices :: Num t => Dimensions t -> [Ix t]
rectangle_corner_indices (nr,nc) = [(0,0),(0,nc - 1),(nr - 1,0),(nr - 1,nc - 1)]

-- | Trapezium given as dimensions with offset for lower indices.
--
-- > trapezium_corner_indices ((2,3),2) == [(0,0),(0,2),(1,2),(1,4)]
trapezium_corner_indices :: Num t => (Dimensions t,t) -> [Ix t]
trapezium_corner_indices ((nr,nc),o) = [(0,0),(0,nc - 1),(nr - 1,o),(nr - 1,nc + o - 1)]

all_ix_translations :: Integral t => Dimensions t -> [Ix t] -> [[Ix t]]
all_ix_translations (nr,nc) ix =
    let f z = ix_modulo (nr,nc) . ix_translate z
    in [map (f (dr,dc)) ix | dr <- [0 .. nr - 1], dc <- [0 .. nc - 1]]
