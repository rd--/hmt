module Music.Theory.Array where

import qualified Data.Array as A {- array -}

import qualified Music.Theory.List as T {- hmt -}

-- * List Array

larray_bounds :: Ord i => [(i,e)] -> (i,i)
larray_bounds = T.minmax . map fst

larray :: A.Ix i => [(i,e)] -> A.Array i e
larray a = A.array (larray_bounds a) a
