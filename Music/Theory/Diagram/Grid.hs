-- | Functions for drawing grid and table structure common in music
-- theory and in compositions such as Morton Feldman's durational
-- /grid/ music of the 1950's.
module Music.Theory.Diagram.Grid where

import qualified Text.HTML.Light as H {- html-minimalist -}
import qualified Text.HTML.Light.Common as H
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

-- | Table of row order 'Table_Cell's.
type Table = [[Table_Cell]]

-- | Construct a 'Table' with one 'X.Content' per cell.
simple_table :: [[X.Content]] -> Table
simple_table = map (map (\x -> ([],[x])))

-- | Construct a 'Table' with one 'X.Content' per cell, and an
-- associated class.
simple_table_class :: [[(String,X.Content)]] -> Table
simple_table_class = map (map (\(c,x) -> ([H.class' c],[x])))

-- | Build a table of @(rows,columns)@ dimensions given a function
-- from @(row,column)@ to 'Table_Cell'.
build_table :: (Int,Int) -> ((Int,Int) -> Table_Cell) -> Table
build_table (m,n) f =
    let mk_row i = map (\j -> f (i,j)) [0 .. n - 1]
    in map mk_row [0 .. m - 1]

-- | Render 'Table' as @HTML@ table.
table :: Table -> X.Content
table t =
    let mk_r = H.tr [] . map (uncurry H.td)
    in H.div [] [H.table [] (map mk_r t)]

-- | Render set of 'Table's as @HTML@.
page :: [Table] -> String
page xs = do
    let tb = map table xs
        bd = H.body [] tb
        css = H.link_css "all" "css/grid.css"
        hd = H.head [] [css]
        e = H.html [H.lang "en"] [hd, bd]
    H.renderHTML5 e

-- | Write set of 'Table's to @HTML@ file.
to_html :: FilePath -> [Table] -> IO ()
to_html o_fn = writeFile o_fn . page
