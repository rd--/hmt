-- | Regular matrix array data, CSV, column & row indexing.
module Music.Theory.Array.CSV where

import Data.List {- base -}

import qualified Data.Array as A {- array -}
import qualified Safe {- safe -}
import qualified Text.CSV.Lazy.String as C {- lazy-csv -}

import qualified Music.Theory.Array as T {- hmt -}
import qualified Music.Theory.Array.Cell_Ref as R {- hmt -}
import qualified Music.Theory.IO as T {- hmt -}
import qualified Music.Theory.List as T {- hmt -}
import qualified Music.Theory.Tuple as T {- hmt -}

-- * FIELD / QUOTE

-- | Quoting is required is the string has a double-quote, comma newline or carriage-return.
csv_requires_quote :: String -> Bool
csv_requires_quote = any (`elem` "\",\n\r")

-- | Quoting places double-quotes at the start and end and escapes double-quotes.
csv_quote :: String -> String
csv_quote fld =
  let esc s =
        case s of
          [] -> []
          '"':s' -> '"' : '"' : esc s'
          c:s' -> c : esc s'
  in '"' : esc fld ++ "\""

-- | Quote field if required.
csv_quote_if_req :: String -> String
csv_quote_if_req fld = if csv_requires_quote fld then csv_quote fld else fld

-- * TABLE

-- | When reading a CSV file is the first row a header?
type CSV_Has_Header = Bool

-- | Alias for 'Char', allow characters other than @,@ as delimiter.
type CSV_Delimiter = Char

-- | Alias for 'Bool', allow linebreaks in fields.
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

-- | CSV table, ie. a 'Table' with 'Maybe' a header.
type CSV_Table a = (Maybe [String],T.Table a)

-- | Read 'CSV_Table' from @CSV@ file.
csv_table_read :: CSV_Opt -> (String -> a) -> FilePath -> IO (CSV_Table a)
csv_table_read (hdr,delim,brk,_) f fn = do
  s <- T.read_file_utf8 fn
  let t = C.csvTable (C.parseDSV brk delim s)
      p = C.fromCSVTable t
      (h,d) = if hdr then (Just (head p),tail p) else (Nothing,p)
  return (h,map (map f) d)

-- | Read 'T.Table' only with 'def_csv_opt'.
csv_table_read_def :: (String -> a) -> FilePath -> IO (T.Table a)
csv_table_read_def f = fmap snd . csv_table_read def_csv_opt f

-- | Read plain CSV 'T.Table'.
csv_table_read_plain :: FilePath -> IO (T.Table String)
csv_table_read_plain = csv_table_read_def id

-- | Read and process @CSV@ 'CSV_Table'.
csv_table_with :: CSV_Opt -> (String -> a) -> FilePath -> (CSV_Table a -> b) -> IO b
csv_table_with opt f fn g = fmap g (csv_table_read opt f fn)

-- | Align table according to 'CSV_Align_Columns'.
--
-- > csv_table_align CSV_No_Align [["a","row","and"],["then","another","one"]]
csv_table_align :: CSV_Align_Columns -> T.Table String -> T.Table String
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

-- | Write 'Table' only (no header) with 'def_csv_opt'.
csv_table_write_def :: (a -> String) -> FilePath -> T.Table a -> IO ()
csv_table_write_def f fn tbl = csv_table_write f def_csv_opt fn (Nothing,tbl)

-- | Write plain CSV 'Table'.
csv_table_write_plain :: FilePath -> T.Table String -> IO ()
csv_table_write_plain = csv_table_write_def id

-- | @0@-indexed (row,column) cell lookup.
table_lookup :: T.Table a -> (Int,Int) -> a
table_lookup t (r,c) = let ix = Safe.atNote "table_lookup" in (t `ix` r) `ix` c

-- | Row data.
table_row :: T.Table a -> R.Row_Ref -> [a]
table_row t r = Safe.atNote "table_row" t (R.row_index r)

-- | Column data.
table_column :: T.Table a -> R.Column_Ref -> [a]
table_column t c = Safe.atNote "table_column" (transpose t) (R.column_index c)

-- | Lookup value across columns.
table_column_lookup :: Eq a => T.Table a -> (R.Column_Ref,R.Column_Ref) -> a -> Maybe a
table_column_lookup t (c1,c2) e =
    let a = zip (table_column t c1) (table_column t c2)
    in lookup e a

-- | Table cell lookup.
table_cell :: T.Table a -> R.Cell_Ref -> a
table_cell t (c,r) =
    let (r',c') = (R.row_index r,R.column_index c)
    in table_lookup t (r',c')

-- | @0@-indexed (row,column) cell lookup over column range.
table_lookup_row_segment :: T.Table a -> (Int,(Int,Int)) -> [a]
table_lookup_row_segment t (r,(c0,c1)) =
    let r' = Safe.atNote "table_lookup_row_segment" t r
    in take (c1 - c0 + 1) (drop c0 r')

-- | Range of cells from row.
table_row_segment :: T.Table a -> (R.Row_Ref,R.Column_Range) -> [a]
table_row_segment t (r,c) =
    let (r',c') = (R.row_index r,R.column_indices c)
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
table_to_array :: T.Table a -> A.Array R.Cell_Ref a
table_to_array t =
    let nr = length t
        nc = length (Safe.atNote "table_to_array" t 0)
        bnd = (R.cell_ref_minima,(toEnum (nc - 1),nr))
        asc = zip (R.cell_range_row_order bnd) (concat t)
    in A.array bnd asc

-- | 'table_to_array' of 'csv_table_read'.
csv_array_read :: CSV_Opt -> (String -> a) -> FilePath -> IO (A.Array R.Cell_Ref a)
csv_array_read opt f fn = fmap (table_to_array . snd) (csv_table_read opt f fn)

-- * Irregular

csv_field_str :: C.CSVField -> String
csv_field_str f =
    case f of
      C.CSVField _ _ _ _ s _ -> s
      C.CSVFieldError _ _ _ _ _ -> error "csv_field_str"

csv_error_recover :: C.CSVError -> C.CSVRow
csv_error_recover e =
    case e of
      C.IncorrectRow _ _ _ f -> f
      C.BlankLine _ _ _ _ -> []
      _ -> error "csv_error_recover: not recoverable"

csv_row_recover :: Either [C.CSVError] C.CSVRow -> C.CSVRow
csv_row_recover r =
    case r of
      Left [e] -> csv_error_recover e
      Left _ -> error "csv_row_recover: multiple errors"
      Right r' -> r'

-- | Read irregular @CSV@ file, ie. rows may have any number of columns, including no columns.
csv_load_irregular :: (String -> a) -> FilePath -> IO [[a]]
csv_load_irregular f fn = do
  s <- T.read_file_utf8 fn
  return (map (map (f . csv_field_str) . csv_row_recover) (C.parseCSV s))

csv_write_irregular :: (a -> String) -> CSV_Opt -> FilePath -> CSV_Table a -> IO ()
csv_write_irregular f opt fn (hdr,tbl) =
  let tbl' = T.tbl_make_regular_nil "" (map (map f) tbl)
  in T.write_file_utf8 fn (csv_table_pp id opt (hdr,tbl'))

csv_write_irregular_def :: (a -> String) -> FilePath -> T.Table a -> IO ()
csv_write_irregular_def f fn tbl = csv_write_irregular f def_csv_opt fn (Nothing,tbl)

-- * Tuples

type P2_Parser t1 t2 = (String -> t1,String -> t2)

csv_table_read_p2 :: P2_Parser t1 t2 -> CSV_Opt -> FilePath -> IO (Maybe (String,String),[(t1,t2)])
csv_table_read_p2 f opt fn = do
  (hdr,dat) <- csv_table_read opt id fn
  return (fmap T.t2_from_list hdr,map (T.p2_from_list f) dat)

type P5_Parser t1 t2 t3 t4 t5 = (String -> t1,String -> t2,String -> t3,String -> t4,String -> t5)
type P5_Writer t1 t2 t3 t4 t5 = (t1 -> String,t2 -> String,t3 -> String,t4 -> String,t5 -> String)

csv_table_read_p5 :: P5_Parser t1 t2 t3 t4 t5 -> CSV_Opt -> FilePath -> IO (Maybe [String],[(t1,t2,t3,t4,t5)])
csv_table_read_p5 f opt fn = do
  (hdr,dat) <- csv_table_read opt id fn
  return (hdr,map (T.p5_from_list f) dat)

csv_table_write_p5 :: P5_Writer t1 t2 t3 t4 t5 -> CSV_Opt -> FilePath -> (Maybe [String],[(t1,t2,t3,t4,t5)]) -> IO ()
csv_table_write_p5 f opt fn (hdr,dat) = csv_table_write id opt fn (hdr,map (T.p5_to_list f) dat)

csv_table_read_t9 :: (String -> t) -> CSV_Opt -> FilePath -> IO (Maybe [String],[T.T9 t])
csv_table_read_t9 f opt fn = do
  (hdr,dat) <- csv_table_read opt id fn
  return (hdr,map (T.t9_from_list . map f) dat)

