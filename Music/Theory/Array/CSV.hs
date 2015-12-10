-- | Regular matrix array data, CSV, column & row indexing.
module Music.Theory.Array.CSV where

import qualified Data.Array as A {- array -}
import Data.Char {- base -}
import Data.Function {- base -}
import Data.List {- base -}
import Data.String {- base -}

import qualified Text.CSV.Lazy.String as C {- lazy-csv -}

import qualified Music.Theory.IO as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}

-- * Indexing

-- | @A@ indexed case-insensitive column references.  The column
-- following @Z@ is @AA@.
data Column_Ref = Column_Ref {column_ref_string :: String}

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

-- | Column reference to interior index within specified range.  Type
-- specialised 'Data.Ix.index'.
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
-- > in map column_ref [0,25,26,51,52,77,78] == c
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

-- | Cell reference pretty printer.
--
-- > cell_ref_pp ("CC",348) == "CC348"
cell_ref_pp :: Cell_Ref -> String
cell_ref_pp (Column_Ref c,r) = c ++ show r

-- | Translate cell reference to @0@-indexed pair.
--
-- > cell_index ("CC",348) == (80,347)
-- > Data.Ix.index (("AA",1),("ZZ",999)) ("CC",348) == 54293
cell_index :: Cell_Ref -> (Int,Int)
cell_index (c,r) = (column_index c,row_index r)

-- | Type specialised 'Data.Ix.range', cells are in column-order.
--
-- > cell_range (("AA",1),("AC",1)) == [("AA",1),("AB",1),("AC",1)]
--
-- > let r = [("AA",1),("AA",2),("AB",1),("AB",2),("AC",1),("AC",2)]
-- > in cell_range (("AA",1),("AC",2)) == r
--
-- > Data.Ix.range (('A',1),('C',1)) == [('A',1),('B',1),('C',1)]
--
-- > let r = [('A',1),('A',2),('B',1),('B',2),('C',1),('C',2)]
-- > in Data.Ix.range (('A',1),('C',2)) == r
cell_range :: Cell_Range -> [Cell_Ref]
cell_range ((c1,r1),(c2,r2)) =
    [(c,r) |
     c <- column_range (c1,c2)
    ,r <- row_range (r1,r2)]

-- | Variant of 'cell_range' in row-order.
--
-- > let r = [(AA,1),(AB,1),(AC,1),(AA,2),(AB,2),(AC,2)]
-- > in cell_range_row_order (("AA",1),("AC",2)) == r
cell_range_row_order ::  Cell_Range -> [Cell_Ref]
cell_range_row_order ((c1,r1),(c2,r2)) =
    [(c,r) |
     r <- row_range (r1,r2)
    ,c <- column_range (c1,c2)]

-- * TABLE

-- | When reading a CSV file is the first row a header?
type CSV_Has_Header = Bool

type CSV_Delimiter = Char

type CSV_Allow_Linebreaks = Bool

-- | When writing a CSV file should the delimiters be aligned,
-- ie. should columns be padded with spaces, and if so at which side
-- of the data?
data CSV_Align_Columns = CSV_No_Align | CSV_Align_Left | CSV_Align_Right

-- | CSV options.
type CSV_Opt = (CSV_Has_Header,CSV_Delimiter,CSV_Allow_Linebreaks,CSV_Align_Columns)

-- | Default CSV options, no header, comma delimiter, no linebreaks, no alignment.
def_csv_opt :: CSV_Opt
def_csv_opt = (False,',',False,CSV_No_Align)

-- | Plain list representation of a two-dimensional table of /a/ in
-- row-order.  Tables are regular, ie. all rows have equal numbers of
-- columns.
type Table a = [[a]]

-- | CSV table, ie. a table with perhaps a header.
type CSV_Table a = (Maybe [String],Table a)

-- | Read 'CSV_Table' from @CSV@ file.
csv_table_read :: CSV_Opt -> (String -> a) -> FilePath -> IO (CSV_Table a)
csv_table_read (hdr,delim,brk,_) f fn = do
  s <- T.read_file_utf8 fn
  let t = C.csvTable (C.parseDSV brk delim s)
      p = C.fromCSVTable t
      (h,d) = if hdr then (Just (head p),tail p) else (Nothing,p)
  return (h,map (map f) d)

-- | Read 'Table' only with 'def_csv_opt'.
csv_table_read' :: (String -> a) -> FilePath -> IO (Table a)
csv_table_read' f = fmap snd . csv_table_read def_csv_opt f

-- | Read and process @CSV@ 'CSV_Table'.
csv_table_with :: CSV_Opt -> (String -> a) -> FilePath -> (CSV_Table a -> b) -> IO b
csv_table_with opt f fn g = fmap g (csv_table_read opt f fn)

-- | Align table according to 'CSV_Align_Columns'.
--
-- > csv_table_align CSV_No_Align [["a","row","and"],["then","another","one"]]
csv_table_align :: CSV_Align_Columns -> Table String -> Table String
csv_table_align align tbl =
    let c = transpose tbl
        n = map (maximum . map length) c
        ext k s = let pd = replicate (k - length s) ' '
                  in case align of
                       CSV_No_Align -> s
                       CSV_Align_Left -> pd ++ s
                       CSV_Align_Right -> s ++ pd
    in transpose (zipWith (map . ext) n c)

-- | Pretty-print 'CSV_Table'.
csv_table_pp :: (a -> String) -> CSV_Opt -> CSV_Table a -> String
csv_table_pp f (_,delim,brk,align) (hdr,tbl) =
  let tbl' = csv_table_align align (T.mcons hdr (map (map f) tbl))
      (_,t) = C.toCSVTable tbl'
  in C.ppDSVTable brk delim t

-- | 'T.write_file_utf8' of 'csv_table_pp'.
csv_table_write :: (a -> String) -> CSV_Opt -> FilePath -> CSV_Table a -> IO ()
csv_table_write f opt fn csv = T.write_file_utf8 fn (csv_table_pp f opt csv)

-- | Write 'Table' only (no header).
csv_table_write' :: (a -> String) -> CSV_Opt -> FilePath -> Table a -> IO ()
csv_table_write' f opt fn tbl = csv_table_write f opt fn (Nothing,tbl)

-- | @0@-indexed (row,column) cell lookup.
table_lookup :: Table a -> (Int,Int) -> a
table_lookup t (r,c) = (t !! r) !! c

-- | Row data.
table_row :: Table a -> Row_Ref -> [a]
table_row t r = t !! row_index r

-- | Column data.
table_column :: Table a -> Column_Ref -> [a]
table_column t c = transpose t !! column_index c

-- | Lookup value across columns.
table_column_lookup :: Eq a => Table a -> (Column_Ref,Column_Ref) -> a -> Maybe a
table_column_lookup t (c1,c2) e =
    let a = zip (table_column t c1) (table_column t c2)
    in lookup e a

-- | Table cell lookup.
table_cell :: Table a -> Cell_Ref -> a
table_cell t (c,r) =
    let (r',c') = (row_index r,column_index c)
    in table_lookup t (r',c')

-- | @0@-indexed (row,column) cell lookup over column range.
table_lookup_row_segment :: Table a -> (Int,(Int,Int)) -> [a]
table_lookup_row_segment t (r,(c0,c1)) =
    let r' = t !! r
    in take (c1 - c0 + 1) (drop c0 r')

-- | Range of cells from row.
table_row_segment :: Table a -> (Row_Ref,Column_Range) -> [a]
table_row_segment t (r,c) =
    let (r',c') = (row_index r,column_indices c)
    in table_lookup_row_segment t (r',c')

-- * Array

-- | Translate 'Table' to 'Array'.  It is assumed that the 'Table' is
-- regular, ie. all rows have an equal number of columns.
--
-- > let a = table_to_array [[0,1,3],[2,4,5]]
-- > in (bounds a,indices a,elems a)
--
-- > > (((A,1),(C,2))
-- > > ,[(A,1),(A,2),(B,1),(B,2),(C,1),(C,2)]
-- > > ,[0,2,1,4,3,5])
table_to_array :: Table a -> A.Array Cell_Ref a
table_to_array t =
    let nr = length t
        nc = length (t !! 0)
        bnd = (cell_ref_minima,(toEnum (nc - 1),nr))
        asc = zip (cell_range_row_order bnd) (concat t)
    in A.array bnd asc

-- | 'table_to_array' of 'csv_table_read'.
csv_array_read :: CSV_Opt -> (String -> a) -> FilePath -> IO (A.Array Cell_Ref a)
csv_array_read opt f fn = fmap (table_to_array . snd) (csv_table_read opt f fn)
