-- | Regular array data as markdown (MD) tables.
module Music.Theory.Array.MD where

import Data.Char {- base -}
import Data.List {- base -}

import qualified Music.Theory.List as T {- hmt -}

-- | Append /k/ to the right of /l/ until result has /n/ places.
pad_right :: a -> Int -> [a] -> [a]
pad_right k n l = take n (l ++ repeat k)

-- | Append /k/ to each row of /tbl/ as required to be regular (all
-- rows equal length).
make_regular :: a -> [[a]] -> [[a]]
make_regular k tbl =
    let z = maximum (map length tbl)
    in map (pad_right k z) tbl

-- | Delete trailing 'Char' where 'isSpace' holds.
delete_trailing_whitespace :: [Char] -> [Char]
delete_trailing_whitespace = reverse . dropWhile isSpace . reverse

-- | Optional header row then data rows.
type MD_Table t = (Maybe [String],[[t]])

-- | Join second table to right of initial table.
md_table_join :: MD_Table a -> MD_Table a -> MD_Table a
md_table_join (nm,c) (hdr,tbl) =
    let hdr' = fmap (\h -> maybe h (++ h) nm) hdr
        tbl' = map (\(i,r) -> i ++ r) (zip c tbl)
    in (hdr',tbl')

-- | Add a row number column at the front of the table.
md_number_rows :: MD_Table String -> MD_Table String
md_number_rows (hdr,tbl) =
    let hdr' = fmap ("#" :) hdr
        tbl' = map (\(i,r) -> show i : r) (zip [1::Int ..] tbl)
    in (hdr',tbl')

-- | Markdown table, perhaps with header.  Table is in row order.
-- Options are: /pad_left/.
--
-- > md_table_opt False (Nothing,[["a","bc","def"],["ghij","klm","no","p"]])
md_table_opt :: Bool -> MD_Table String -> [String]
md_table_opt pleft (hdr,t) =
    let t' = maybe t (:t) hdr
        c = transpose (make_regular "" t')
        n = map (maximum . map length) c
        ext k s = let pd = replicate (k - length s) ' '
                  in if pleft then pd ++ s else s ++ pd
        m = unwords (map (flip replicate '-') n)
        w = map unwords (transpose (zipWith (map . ext) n c))
        d = map delete_trailing_whitespace w
    in case hdr of
         Nothing -> T.bracket (m,m) d
         Just _ -> case d of
                     [] -> error "md_table"
                     d0:d' -> d0 : T.bracket (m,m) d'

md_table' :: MD_Table String -> [String]
md_table' = md_table_opt True

-- | 'curry' of 'md_table''.
md_table :: Maybe [String] -> [[String]] -> [String]
md_table = curry md_table'

-- | Variant relying on 'Show' instances.
--
-- > md_table_show Nothing [[1..4],[5..8],[9..12]]
md_table_show :: Show t => Maybe [String] -> [[t]] -> [String]
md_table_show hdr = md_table hdr . map (map show)

-- | Variant in column order (ie. 'transpose').
--
-- > md_table_column_order [["a","bc","def"],["ghij","klm","no"]]
md_table_column_order :: Maybe [String] -> [[String]] -> [String]
md_table_column_order hdr = md_table hdr . transpose

-- | Two-tuple 'show' variant.
md_table_p2 :: (Show a,Show b) => Maybe [String] -> ([a],[b]) -> [String]
md_table_p2 hdr (p,q) = md_table hdr [map show p,map show q]

-- | Three-tuple 'show' variant.
md_table_p3 :: (Show a,Show b,Show c) => Maybe [String] -> ([a],[b],[c]) -> [String]
md_table_p3 hdr (p,q,r) = md_table hdr [map show p,map show q,map show r]

{- | Matrix form, ie. header in both first row and first column, in
each case displaced by one location which is empty.

> let h = (map return "abc",map return "efgh")
> let t = md_matrix "" h (map (map show) [[1,2,3,4],[2,3,4,1],[3,4,1,2]])

>>> putStrLn $ unlines $ md_table' t
 - - - - -
  e f g h
a 1 2 3 4
b 2 3 4 1
c 3 4 1 2
- - - - -

-}
md_matrix :: a -> ([a],[a]) -> [[a]] -> MD_Table a
md_matrix nil (r,c) t = md_table_join (Nothing,[nil] : map return r) (Nothing,c : t)

-- | Variant that takes a 'show' function and a /header decoration/ function.
md_matrix_opt :: (a -> String) -> (String -> String) -> ([a],[a]) -> [[a]] -> MD_Table String
md_matrix_opt show_f hd_f nm t =
    let t' = map (map show_f) t
        nm' = T.bimap1 (map (hd_f . show_f)) nm
    in md_matrix "" nm' t'

-- | 'md_matrix_opt' with 'show' and markdown /bold/ annotations for header.
-- the header cells are in bold.
md_matrix_bold :: ([String],[String]) -> [[String]] -> MD_Table String
md_matrix_bold = let bold x = "__" ++ x ++ "__" in md_matrix_opt show bold
