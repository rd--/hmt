module Music.Theory.Diagram.Grid where

import qualified Codec.Binary.UTF8.String as U {- utf8-string -}
import qualified Graphics.Rendering.Cairo as C {- cairo -}
import qualified Text.HTML.Light as H {- html-minimalist -}
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

-- | Render line.
line :: [P] -> C.Render ()
line l =
    case l of
      [] -> return ()
      (x0,y0):l' -> do C.moveTo x0 y0
                       mapM_ (\(x,y) -> C.lineTo x y) l'

-- | Render rectangle given colour 'C', upper-left 'P' and
-- /(width,height)/.
rect :: C -> P -> (R,R) -> C.Render ()
rect c (x,y) (w,h) = do
  let (r,g,b) = c
  C.save
  C.setSourceRGBA r g b 1
  C.setLineWidth 0.05
  C.translate x y
  C.rectangle 0 0 w h
  C.stroke
  C.restore

-- | Render text 'String' in colour 'C' and point 'P' in font size 'R'.
txt_at :: C -> P -> R -> String -> C.Render ()
txt_at c (x,y) sz txt = do
  let (r,g,b) = c
  C.save
  C.selectFontFace "Times" C.FontSlantNormal C.FontWeightNormal
  C.setFontSize sz
  C.setSourceRGBA r g b 1
  C.moveTo x y
  C.showText (U.utf8Encode txt)
  C.restore

-- | Render 'Grid' of /(rows,columns)/ with displacement /(dx,dy)/ in
-- inidcated font size.
mk_grid :: (Int,Int) -> (R,R) -> R -> Grid -> C.Render ()
mk_grid (r,c) (dx,dy) fs xs = do
  let g = grid (10,10) (10,10) (r,c)
      grid_pt' = displace (dx,dy) . grid_pt (10,10) (10,10)
  mapM_ (\(x,y) -> rect (0,0,0) (x,y) (10,10)) g
  mapM_ (\(l,clr,i) -> txt_at clr (grid_pt' l) (10/fs) i) xs
  C.showPage

-- | Make a bounding box from /row/ and /column/ dimensions.
mk_bbox :: (Int,Int) -> (R,R)
mk_bbox (r,c) =
    let f n = (fromIntegral n + 2) * 10
    in (f c,f r)

-- | Run render to @PDF@ file.
to_pdf :: FilePath -> (R,R) -> C.Render () -> IO ()
to_pdf nm (w,h) f = do
  let g s = C.renderWith s f
  C.withPDFSurface nm w h g

-- * Table

-- | Table of row order 'X.Content'.
type Table = [[X.Content]]

-- | Render 'Table' as @XHTML@ table.
table :: Table -> X.Content
table t =
    let mk_c x = H.td [] [x]
        mk_r = H.tr [] . map mk_c
    in H.div [] [H.table [] (map mk_r t)]

-- | Render set of 'Table's as @XHTML@.
page :: [Table] -> String
page xs = do
    let tb = map table xs
        bd = H.body [] tb
        css = H.link [H.rel "stylesheet"
                     ,H.type' "text/css"
                     ,H.href "css/grid.css"]
        hd = H.head [] [css]
        e = H.html [H.xmlns "http://www.w3.org/1999/xhtml"] [hd, bd]
    H.renderXHTML H.xhtml_1_0_strict e

-- | Write set of 'Table's to @XHTML@ file.
to_xhtml :: FilePath -> [Table] -> IO ()
to_xhtml o_fn = writeFile o_fn . page
