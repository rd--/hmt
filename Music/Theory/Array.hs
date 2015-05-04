module Music.Theory.Array where

import Data.Array {- array -}

import Music.Theory.List {- hmt -}

-- * List Array

larray_bounds :: Ord i => [(i,e)] -> (i,i)
larray_bounds = minmax . map fst

larray :: Ix i => [(i,e)] -> Array i e
larray a = array (larray_bounds a) a
