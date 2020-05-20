import Data.IORef {- base -}
import Data.List.Split {- base -}
import System.Exit {- base -}

import Graphics.UI.GLUT {- GLUT -}

import qualified Music.Theory.Graph.OBJ as T {- hmt -}
import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.Opt as T {- hmt -}

-- * GEOMETRY

type R = GLfloat
type P3 = (R,R,R)

-- * GR/OBJ

type GR = T.LBL P3 ()

obj_load :: FilePath -> IO GR
obj_load = T.obj_load_v3_graph

type LN = [[Vertex3 R]]

gr_to_ln :: GR -> LN
gr_to_ln (v,e) =
  let ix = maybe (error "?") id . flip lookup v
      f (x,y,z) = Vertex3 x y z
      g ((i,j),_) = map f [ix i,ix j]
  in chunksOf 16 (concatMap g e) -- does sending vertices in chunks help?

-- * IOREF

withIORef :: IORef a -> (a -> IO ()) -> IO ()
withIORef s f = readIORef s >>= f

-- * STATE

type R3 = (R,R,R)

-- | R=zoom R3=rotation-(x,y,z)
data State = State R R3

mod_rot_f :: R3 -> State -> State
mod_rot_f (dx,dy,dz) (State s (x,y,z)) = State s (x + dx,y + dy,z + dz)

mod_rot :: IORef State -> R3 -> IO ()
mod_rot s d = modifyIORef s (mod_rot_f d)

mod_zoom_f :: R -> State -> State
mod_zoom_f n (State s r) = State (s + n) r

mod_zoom :: IORef State -> R -> IO ()
mod_zoom s n = modifyIORef s (mod_zoom_f n)

-- * GL

{-
gl_with_time :: String -> IO x -> IO ()
gl_with_time m x = do
  t0 <- elapsedTime
  _ <- x
  t1 <- elapsedTime
  print (m,t1 - t0)
-}

gl_render_ln :: LN -> IO ()
gl_render_ln = mapM_ (\x -> renderPrimitive Lines (mapM_ vertex x))

gl_grey :: Color4 R
gl_grey = Color4 0.5 0.5 0.5 0.5

gl_render_state :: State -> IO ()
gl_render_state (State s (x,y,z)) = do
    rotate x (Vector3 1 0 0)
    rotate y (Vector3 0 1 0)
    rotate z (Vector3 0 0 1)
    scale s s s
    color gl_grey

gl_draw :: LN -> State -> IO ()
gl_draw ln s = do
  clear [ColorBuffer]
  preservingMatrix (gl_render_state s >> gl_render_ln ln)
  swapBuffers

gl_keyboard :: IORef State -> Key -> KeyState -> Modifiers -> Position -> IO ()
gl_keyboard s c _ _ _ =
  case c of
    Char 'z' -> mod_rot s ( 0, 0, 5)
    Char 'Z'-> mod_rot s ( 0, 0,-5)
    SpecialKey KeyUp -> mod_rot s ( 5, 0, 0)
    SpecialKey KeyDown -> mod_rot s (-5, 0, 0)
    SpecialKey KeyLeft -> mod_rot s ( 0, 5, 0)
    SpecialKey KeyRight -> mod_rot s ( 0,-5, 0)
    Char '+' -> mod_zoom s 0.01
    Char '-' -> mod_zoom s (-0.01)
    SpecialKey KeyPageUp -> mod_zoom s 0.05
    SpecialKey KeyPageDown -> mod_zoom s (-0.05)
    Char 'Q' -> exitWith ExitSuccess
    _ -> return ()

gl_init :: IO ()
gl_init = do
  viewport $= (Position 0 0,Size 800 800)
  matrixMode $= Projection
  loadIdentity
  perspective 30 1 1 100
  matrixMode $= Modelview 0
  loadIdentity
  lookAt (Vertex3 0 0 10) (Vertex3 0 0 0) (Vector3 0 1 0)
  shadeModel $= Smooth
  clearColor $= Color4 0 0 0 0
  blend $= Enabled
  blendFunc $= (SrcAlpha,One)
  depthFunc $= Nothing -- Just Less
  lighting $= Disabled
  normalize $= Enabled

timer_f :: Timeout -> IO ()
timer_f dly = do
  postRedisplay Nothing
  addTimerCallback dly (timer_f dly)

gl_gr_obj :: (GLsizei, GLsizei) -> Timeout -> FilePath -> IO ()
gl_gr_obj (w,h) dly fn = do
  ln <- fmap gr_to_ln (obj_load fn)
  _ <- initialize fn []
  initialDisplayMode $= [RGBAMode,DoubleBuffered]
  initialWindowSize $= Size w h
  initialWindowPosition $= Position 0 0
  _ <- createWindow "GL"
  gl_init
  s <- newIORef (State 1.0 (20,30,0))
  displayCallback $= withIORef s (gl_draw ln)
  keyboardMouseCallback $= Just (gl_keyboard s)
  addTimerCallback dly (timer_f dly)
  mainLoop

usg :: T.OPT_USG
usg = ["obj-gr [opt] file-name"]

opt :: [T.OPT_USR]
opt =
  [("delay","100","int","timer delay (ms)")
  ,("height","400","int","window height (px)")
  ,("width","400","int","window width (px)")]

main :: IO ()
main = do
  (o,a) <- T.opt_get_arg True usg opt
  case a of
    ["obj-gr",fn] -> gl_gr_obj (T.opt_read o "width",T.opt_read o "height") (T.opt_read o "delay") fn
    _ -> T.opt_usage usg opt

