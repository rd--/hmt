-- | Cell references & indexing.
module Music.Theory.Array.Cell_Ref where

import Data.Char {- base -}
import Data.Function {- base -}
import Data.Maybe {- base -}
import Data.String {- base -}

import qualified Data.Array as A {- array -}

-- | @A@ indexed case-insensitive column references.  The column following @Z@ is @AA@.
newtype Column_Ref = Column_Ref {column_ref_string :: String}

instance IsString Column_Ref where fromString = Column_Ref
instance Read Column_Ref where readsPrec _ s = [(Column_Ref s,[])]
instance Show Column_Ref where show = column_ref_string
instance Eq Column_Ref where (==) = (==) `on` column_index
instance Ord Column_Ref where compare = compare `on` column_index

instance Enum Column_Ref where
    fromEnum = column_index
    toEnum = column_ref

instance A.Ix Column_Ref where
    range = column_range
    index = interior_column_index
    inRange = column_in_range
    rangeSize = column_range_size

-- | Inclusive range of column references.
type Column_Range = (Column_Ref,Column_Ref)

-- | @1@-indexed row reference.
type Row_Ref = Int

-- | Zero index of 'Row_Ref'.
row_index :: Row_Ref -> Int
row_index r = r - 1

-- | Inclusive range of row references.
type Row_Range = (Row_Ref,Row_Ref)

-- | Cell reference, column then row.
type Cell_Ref = (Column_Ref,Row_Ref)

-- | Inclusive range of cell references.
type Cell_Range = (Cell_Ref,Cell_Ref)

-- | Case folding letter to index function.  Only valid for ASCII letters.
--
-- > map letter_index ['A' .. 'Z'] == [0 .. 25]
-- > map letter_index ['a','d' .. 'm'] == [0,3 .. 12]
letter_index :: Char -> Int
letter_index c = fromEnum (toUpper c) - fromEnum 'A'

-- | Inverse of 'letter_index'.
--
-- > map index_letter [0,3 .. 12] == ['A','D' .. 'M']
index_letter :: Int -> Char
index_letter i = toEnum (i + fromEnum 'A')

-- | Translate column reference to @0@-index.
--
-- > :set -XOverloadedStrings
-- > map column_index ["A","c","z","ac","XYZ"] == [0,2,25,28,17575]
column_index :: Column_Ref -> Int
column_index (Column_Ref c) =
    let m = iterate (* 26) 1
        i = reverse (map letter_index c)
    in sum (zipWith (*) m (zipWith (+) [0..] i))

-- | Column reference to interior index within specified range.
--   Type specialised 'Data.Ix.index'.
--
-- > map (Data.Ix.index ('A','Z')) ['A','C','Z'] == [0,2,25]
-- > map (interior_column_index ("A","Z")) ["A","C","Z"] == [0,2,25]
--
-- > map (Data.Ix.index ('B','C')) ['B','C'] == [0,1]
-- > map (interior_column_index ("B","C")) ["B","C"] == [0,1]
interior_column_index :: Column_Range -> Column_Ref -> Int
interior_column_index (l,r) c =
    let n = column_index c
        l' = column_index l
        r' = column_index r
    in if n > r'
       then error (show ("interior_column_index",l,r,c))
       else n - l'

-- | Inverse of 'column_index'.
--
-- > let c = ["A","Z","AA","AZ","BA","BZ","CA"]
-- > map column_ref [0,25,26,51,52,77,78] == c
--
-- > column_ref (0+25+1+25+1+25+1) == "CA"
column_ref :: Int -> Column_Ref
column_ref =
    let rec n = case n `quotRem` 26 of
                  (0,r) -> [index_letter r]
                  (q,r) -> index_letter (q - 1) : rec r
    in Column_Ref . rec

-- | Type specialised 'pred'.
--
-- > column_ref_pred "DF" == "DE"
column_ref_pred :: Column_Ref -> Column_Ref
column_ref_pred = pred

-- | Type specialised 'succ'.
--
-- > column_ref_succ "DE" == "DF"
column_ref_succ :: Column_Ref -> Column_Ref
column_ref_succ = succ

-- | Bimap of 'column_index'.
--
-- > column_indices ("b","p") == (1,15)
-- > column_indices ("B","IT") == (1,253)
column_indices :: Column_Range -> (Int,Int)
column_indices =
    let bimap f (i,j) = (f i,f j)
    in bimap column_index

-- | Type specialised 'Data.Ix.range'.
--
-- > column_range ("L","R") == ["L","M","N","O","P","Q","R"]
-- > Data.Ix.range ('L','R') == "LMNOPQR"
column_range :: Column_Range -> [Column_Ref]
column_range rng =
    let (l,r) = column_indices rng
    in map column_ref [l .. r]

-- | Type specialised 'Data.Ix.inRange'.
--
-- > map (column_in_range ("L","R")) ["A","N","Z"] == [False,True,False]
-- > map (column_in_range ("L","R")) ["L","N","R"] == [True,True,True]
--
-- > map (Data.Ix.inRange ('L','R')) ['A','N','Z'] == [False,True,False]
-- > map (Data.Ix.inRange ('L','R')) ['L','N','R'] == [True,True,True]
column_in_range :: Column_Range -> Column_Ref -> Bool
column_in_range rng c =
    let (l,r) = column_indices rng
        k = column_index c
    in k >= l && k <= r

-- | Type specialised 'Data.Ix.rangeSize'.
--
-- > map column_range_size [("A","Z"),("AA","ZZ")] == [26,26 * 26]
-- > Data.Ix.rangeSize ('A','Z') == 26
column_range_size :: Column_Range -> Int
column_range_size = (+ 1) . negate . uncurry (-) . column_indices

-- | Type specialised 'Data.Ix.range'.
row_range :: Row_Range -> [Row_Ref]
row_range = A.range

-- | The standard uppermost leftmost cell reference, @A1@.
--
-- > Just cell_ref_minima == parse_cell_ref "A1"
cell_ref_minima :: Cell_Ref
cell_ref_minima = (Column_Ref "A",1)

-- | Cell reference parser for standard notation of (column,row).
--
-- > parse_cell_ref "CC348" == Just ("CC",348)
parse_cell_ref :: String -> Maybe Cell_Ref
parse_cell_ref s =
    case span isUpper s of
      ([],_) -> Nothing
      (c,r) -> case span isDigit r of
                 (n,[]) -> Just (Column_Ref c,read n)
                 _ -> Nothing

-- | 'isJust' of 'parse_cell_ref'.
is_cell_ref :: String -> Bool
is_cell_ref = isJust . parse_cell_ref

-- | 'fromJust' of 'parse_cell_ref'
parse_cell_ref_err :: String -> Cell_Ref
parse_cell_ref_err = fromMaybe (error "parse_cell_ref") . parse_cell_ref

-- | Cell reference pretty printer.
--
-- > cell_ref_pp ("CC",348) == "CC348"
cell_ref_pp :: Cell_Ref -> String
cell_ref_pp (Column_Ref c,r) = c ++ show r

-- | Translate cell reference to @0@-indexed pair.
--
-- > cell_index ("CC",348) == (80,347)
-- > Data.Ix.index ((Column_Ref "AA",1),(Column_Ref "ZZ",999)) (Column_Ref "CC",348) == 54293
cell_index :: Cell_Ref -> (Int,Int)
cell_index (c,r) = (column_index c,row_index r)

-- | Inverse of cell_index.
--
-- > index_to_cell (80,347) == (Column_Ref "CC",348)
-- > index_to_cell (4,5) == (Column_Ref "E",6)
index_to_cell :: (Int,Int) -> Cell_Ref
index_to_cell (c,r) = (column_ref c,r + 1)

-- | 'cell_index' of 'parse_cell_ref_err'
parse_cell_index :: String -> (Int,Int)
parse_cell_index = cell_index . parse_cell_ref_err

-- | Type specialised 'Data.Ix.range', cells are in column-order.
--
-- > cell_range (("AA",1),("AC",1)) == [("AA",1),("AB",1),("AC",1)]
--
-- > let r = [("AA",1),("AA",2),("AB",1),("AB",2),("AC",1),("AC",2)]
-- > cell_range (("AA",1),("AC",2)) == r
--
-- > Data.Ix.range (('A',1),('C',1)) == [('A',1),('B',1),('C',1)]
--
-- > let r = [('A',1),('A',2),('B',1),('B',2),('C',1),('C',2)]
-- > Data.Ix.range (('A',1),('C',2)) == r
cell_range :: Cell_Range -> [Cell_Ref]
cell_range ((c1,r1),(c2,r2)) =
    [(c,r) |
     c <- column_range (c1,c2)
    ,r <- row_range (r1,r2)]

-- | Variant of 'cell_range' in row-order.
--
-- > let r = [("AA",1),("AB",1),("AC",1),("AA",2),("AB",2),("AC",2)]
-- > cell_range_row_order (("AA",1),("AC",2)) == r
cell_range_row_order ::  Cell_Range -> [Cell_Ref]
cell_range_row_order ((c1,r1),(c2,r2)) =
    [(c,r) |
     r <- row_range (r1,r2)
    ,c <- column_range (c1,c2)]
