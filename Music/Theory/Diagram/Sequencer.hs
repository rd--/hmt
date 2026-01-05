-- | Sequencer type diagram.
module Music.Theory.Diagram.Sequencer where

import Data.Char {- base -}
import System.FilePath {- filepath -}
import System.Process {- process -}
import Text.Printf {- base -}

import qualified Music.Theory.Amplitude as Amplitude {- hmt-base -}
import qualified Music.Theory.Array.Csv.Midi.Mnd as Mnd {- hmt-base -}
import qualified Music.Theory.Geometry.Vector as Vector {- hmt-base -}

import qualified Music.Theory.Time.Seq as Seq {- hmt -}

-- | Point
type P2 = Vector.V2 Double

-- | Greyscale colour.
type Grey = Double

-- | Coloured rectangle as (lower-left,upper-right,greyscale).
type C_Rect = (P2, P2, Grey)

-- | 'C_Rect' with identifier.
type K_Rect = (Int, C_Rect)

-- | Gnuplot string for 'K_Rect'.
k_rect_gnuplot :: K_Rect -> String
k_rect_gnuplot (i, ((x0, y0), (x1, y1), c)) =
  let fmt = "set object %d rect from %f,%f to %f,%f fc rgbcolor \"#%02x%02x%02x\""
      c' = floor (c * 255) :: Int
  in printf fmt i x0 y0 x1 y1 c' c' c'

{- | Sequencer plot options, (image-size(w,h),x-range,y-range).  For
standard midi data x-range is the time window and y-range is the
gamut.
-}
type Seq_Plot_Opt = (Vector.V2 Int, Vector.V2 Double, Vector.V2 Double)

-- | Sane defaults for size and gamut.
default_seq_plot_opt :: Vector.V2 Double -> Seq_Plot_Opt
default_seq_plot_opt x = ((1200, 400), x, (21, 108))

-- | Add identifiers.
to_k_rect :: [C_Rect] -> [K_Rect]
to_k_rect = zip [1 ..]

-- | Names for Svg terminal have character restrictions.
clean_name :: String -> String
clean_name =
  let f c = if isAlphaNum c then c else '_'
  in map f

-- | Arbitrary Gnuplot commands can be given.
type Gnuplot_Opt = [String]

-- | Options product.
type Opt = (Gnuplot_Opt, Seq_Plot_Opt)

sequencer_plot_rect :: Opt -> FilePath -> String -> [C_Rect] -> IO ()
sequencer_plot_rect (gopt, ((w, h), (x0, x1), (y0, y1))) fs_dir fs_nm sq = do
  let nm_plot = fs_dir </> fs_nm <.> "plot"
      nm_svg = fs_dir </> fs_nm <.> "svg"
      x_range = concat ["[", show x0, ":", show x1, "]"]
      y_range = concat ["[", show y0, ":", show y1, "]"]
      pre =
        [ concat ["set terminal svg name \"", clean_name fs_nm, "\" size ", show w, ",", show h]
        , "set output '" ++ nm_svg ++ "'"
        , "set tics font \"cmr10, 10\""
        , "unset key"
        , concat ["set xrange ", x_range]
        , concat ["set yrange ", y_range]
        , "set bars 0"
        ]
          ++ gopt
      post = ["plot \"/dev/null\" with xyerrorbars lc rgbcolor \"black\""]
  writeFile nm_plot (unlines (pre ++ map k_rect_gnuplot (to_k_rect sq) ++ post))
  _ <- system ("gnuplot " ++ nm_plot)
  return ()

-- * Midi

{- | Linear amplitude to grey scale (0 = white, 1 = black).

>>> map (floor . (* 255) . amp_to_grey (-60)) [0,0.25,0.5,0.75,1]
[255,51,25,10,0]
-}
amp_to_grey :: (Floating t, Ord t) => t -> t -> t
amp_to_grey z am =
  let db = max (Amplitude.amp_db am) z
      z' = abs z
  in 1 - ((db + z') / z')

-- | Midi velocity number to linear amplitude.
vel_to_amp :: Int -> Double
vel_to_amp vel = fromIntegral vel / 127

-- | Midi velocity number to grey scale.
vel_to_grey :: Double -> Int -> Double
vel_to_grey z = amp_to_grey z . vel_to_amp

-- | Midi sequence data, data is (midi-note-number,midi-velocity,midi-channel,param).
type Sequencer_Midi n = Seq.Wseq Double (Mnd.Event n)

-- | Is the drawing 'Horizontal' (left to right) or 'Vertical' (descending).
data Seq_Dir = Horizontal | Vertical

{- | Convert 'Sequencer_Midi' node to 'C_Rect'.  The extent is only
used for 'Vertical' drawings (the Gnuplot co-ordinate system is
ascending and the drawing is descending).  The midi-channel data is
ignored (at present...).
-}
sequencer_midi_to_rect :: Real n => (Seq_Dir, Double) -> (Vector.V2 Double, Mnd.Event n) -> C_Rect
sequencer_midi_to_rect (dir, ext) ((st, du), (mnn, vel, _ch, _pm)) =
  let x0 = st
      x1 = st + du
      y0 = realToFrac mnn
      y1 = y0 + 1
      vel' = realToFrac vel :: Double
      c = vel_to_grey (-60) (floor vel')
  in case dir of
      Horizontal -> ((x0, y0), (x1, y1), c)
      Vertical -> ((y0, ext - x1), (y1, ext - x0), c)

-- | Plot 'Sequencer_Midi'.
sequencer_plot_midi_dir :: Real n => Seq_Dir -> Opt -> FilePath -> String -> Sequencer_Midi n -> IO ()
sequencer_plot_midi_dir dir opt fs_dir fs_nm =
  let (ext, opt') = case dir of
        Horizontal -> (undefined, opt)
        Vertical -> let (p, ((w, h), x, y)) = opt in (snd x, (p, ((h, w), y, x)))
  in sequencer_plot_rect opt' fs_dir fs_nm
      . map (sequencer_midi_to_rect (dir, ext))

-- | 'sequencer_plot_midi_dir' 'Horizontal'.
sequencer_plot_midi :: Real n => Opt -> FilePath -> String -> Sequencer_Midi n -> IO ()
sequencer_plot_midi = sequencer_plot_midi_dir Horizontal
