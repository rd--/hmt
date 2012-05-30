-- | Functions for drawing grid and table structure common in music
-- theory and in compositions such as Morton Feldman's durational
-- /grid/ music of the 1950's.
module Music.Theory.Diagram.Grid where

import Data.Maybe
import qualified Text.HTML.Light as H {- html-minimalist -}
import qualified Text.HTML.Light.Composite as H
import qualified Text.XML.Light as X {- xml -}

-- * Grid

-- | Real number, synonym for 'Double'.
type R = Double

-- | Point given as pair of 'R'.
type P = (R,R)

-- | Red, green and blue colour triple.
type C = (R,R,R)

-- | Cell location as row and column indices.
type L = (Int,Int)

-- | Cell
type Cell = (L,C,String)

-- | Grid
type Grid = [Cell]

-- | Given /(x,y)/ upper-left co-ordinate of grid, /(w,h)/ cell
-- dimensions, and /(r,c)/ grid dimensions, make list of upper-left
-- co-ordinates of cells.
--
-- > grid (10,10) (50,10) (2,2) == [(10,10),(60,10),(10,20),(60,20)]
grid :: P -> (R,R) -> (Int,Int) -> [P]
grid (x,y) (w,h) (r,c) =
    let xs = take c [x, x + w ..]
        ys = take r [y, y + h ..]
    in concatMap (zip xs . repeat) ys

-- | Variant on 'grid' that constructs a single point.
--
-- > map (grid_pt (10,10) (50,10)) [(0,0),(1,1)] == [(10,10),(60,20)]
grid_pt :: (R,R) -> (R,R) -> L -> P
grid_pt (x,y) (w,h) (r,c) =
    let r' = fromIntegral r
        c' = fromIntegral c
    in (x + c' * w,y + r' * h)

-- | Displace 'P' (pointwise addition).
--
-- > displace (2,3) (1,1) == (3,4)
displace :: (R,R) -> P -> P
displace (dx,dy) (x,y) = (x+dx,y+dy)

-- | Make a bounding box from /row/ and /column/ dimensions.
mk_bbox :: (Int,Int) -> (R,R)
mk_bbox (r,c) =
    let f n = (fromIntegral n + 2) * 10
    in (f c,f r)

-- * Table

-- | A table cell is an 'X.Attr' and 'X.Content' duple.
type Table_Cell = ([X.Attr],[X.Content])

type Caption = [X.Content]

-- | Table of row order 'Table_Cell's.
type Table = (Caption,[[Table_Cell]])

-- | Construct a 'Table' with one 'X.Content' per cell.
simple_table :: Caption -> [[X.Content]] -> Table
simple_table c z = (c,map (map (\x -> ([],[x]))) z)

-- | Construct a 'Table' with one 'X.Content' per cell, and an
-- associated class.
simple_table_class :: Caption -> [[(String,X.Content)]] -> Table
simple_table_class c z = (c,map (map (\(nm,x) -> ([H.class' nm],[x]))) z)

type Build_F = ((Int,Int) -> Maybe Table_Cell)

-- | Build a table of @(rows,columns)@ dimensions given a function
-- from @(row,column)@ to 'Maybe' 'Table_Cell'.  If the function is
-- 'Nothing' the cell is skipped, becase another cell has claimed it's
-- locations with 'H.colspan' or 'H.rowspan'.
build_table_m :: Caption -> (Int,Int) -> Build_F -> Table
build_table_m c (m,n) f =
    let mk_row i = mapMaybe (\j -> f (i,j)) [0 .. n - 1]
    in (c,map mk_row [0 .. m - 1])

-- | Build a table of @(rows,columns)@ dimensions given a function
-- from @(row,column)@ to 'Table_Cell'.
build_table :: Caption -> (Int,Int) -> ((Int,Int) -> Table_Cell) -> Table
build_table c (m,n) f = build_table_m c (m,n) (Just . f)

-- | Render 'Table' as @HTML@ table.
table :: Table -> X.Content
table (c,z) =
    let mk_r = H.tr [] . map (uncurry H.td)
    in H.table [] (H.caption [] c : map mk_r z)

-- | A set of related tables.
type Table_Set = [Table]

-- | Render a 'Table_Set's in a @div@ with class @table-set@.
table_set :: Table_Set -> X.Content
table_set = H.div [H.class' "table-set"] . map table

-- | Render set of 'Table_Set's as @HTML@.
page :: Maybe FilePath -> [Table_Set] -> String
page css xs = do
    let tb = map table_set xs
        bd = H.body [H.class' "table-page"] tb
        css' = H.link_css "all" (fromMaybe "css/grid.css" css)
        hd = H.head [] [css']
        e = H.html [H.lang "en"] [hd, bd]
    H.renderHTML5 e

-- | Write set of 'Table_Set's to @HTML@ file.
to_html :: FilePath -> Maybe FilePath -> [Table_Set] -> IO ()
to_html o_fn css = writeFile o_fn . page css
