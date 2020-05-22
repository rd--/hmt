{-
GLU  = <https://www.khronos.org/registry/OpenGL/specs/gl/glu1.3.pdf>
GLUT = <https://www.opengl.org/resources/libraries/glut/glut-3.spec.pdf>
-}

import Data.IORef {- base -}
import Data.List.Split {- base -}
import System.Exit {- base -}

import Graphics.UI.GLUT {- GLUT -}

import qualified Music.Theory.Graph.OBJ as T {- hmt -}
import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.Opt as T {- hmt -}

-- * GEOMETRY

type R = GLfloat
type R3 = (R,R,R)

-- * GR/OBJ

type GR = T.LBL R3 ()

obj_load :: FilePath -> IO GR
obj_load = T.obj_load_v3_graph

type LN = [[Vertex3 R]]

gr_to_ln :: GR -> LN
gr_to_ln (v,e) =
  let ix = maybe (error "?") id . flip lookup v
      f (x,y,z) = Vertex3 x y z
      g ((i,j),_) = map f [ix i,ix j]
  in chunksOf 16 (concatMap g e) -- does sending vertices in chunks help?

gr_load_set :: [FilePath] -> IO LN
gr_load_set fn = do
  g <- mapM (fmap gr_to_ln . obj_load) fn
  return (concat g)

-- * IOREF

withIORef :: IORef a -> (a -> IO ()) -> IO ()
withIORef s f = readIORef s >>= f

-- * STATE

-- | (zoom,rotation-(x,y,z),translation-(x,y,z))
type State = (R,R3,R3)

state_0 :: State
state_0 = (1.0,(20,30,0),(0,0,0))

mod_trs_f :: R3 -> State -> State
mod_trs_f (dx,dy,dz) (s,r,(x,y,z)) = (s,r,(x + dx,y + dy,z + dz))

mod_trs :: IORef State -> R3 -> IO ()
mod_trs s d = modifyIORef s (mod_trs_f d)

mod_rot_f :: R3 -> State -> State
mod_rot_f (dx,dy,dz) (s,(x,y,z),t) = (s,(x + dx,y + dy,z + dz),t)

mod_rot :: IORef State -> R3 -> IO ()
mod_rot s d = modifyIORef s (mod_rot_f d)

mod_zoom_f :: R -> State -> State
mod_zoom_f n (s,r,t) = (s + n,r,t)

mod_zoom :: IORef State -> R -> IO ()
mod_zoom s n = modifyIORef s (mod_zoom_f n)

mod_init :: IORef State -> IO ()
mod_init s = modifyIORef s (const state_0)

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
gl_render_state (sc,(rx,ry,rz),(tx,ty,tz)) = do
    translate (Vector3 tx ty tz)
    rotate rx (Vector3 1 0 0)
    rotate ry (Vector3 0 1 0)
    rotate rz (Vector3 0 0 1)
    scale sc sc sc
    color gl_grey

gl_draw :: LN -> State -> IO ()
gl_draw ln s = do
  clear [ColorBuffer]
  preservingMatrix (gl_render_state s >> gl_render_ln ln)
  swapBuffers

gl_keyboard :: IORef State -> Key -> KeyState -> Modifiers -> Position -> IO ()
gl_keyboard s c _ m _ =
  case (c,ctrl m == Down) of
    (SpecialKey KeyDown,ctl) -> if ctl then mod_trs s (0,-0.1,0) else mod_rot s (-5,0,0)
    (SpecialKey KeyUp,ctl) -> if ctl then mod_trs s (0,0.1,0) else mod_rot s (5,0,0)
    (SpecialKey KeyLeft,ctl) -> if ctl then mod_trs s (-0.1,0,0) else mod_rot s (0,-5,0)
    (SpecialKey KeyRight,ctl) -> if ctl then mod_trs s (0.1,0,0) else mod_rot s (0,5,0)
    (SpecialKey KeyPageUp,ctl) -> if ctl then mod_trs s (0,0,0.1) else mod_rot s (0,0,5)
    (SpecialKey KeyPageDown,ctl) -> if ctl then mod_trs s (0,0,-0.1) else mod_rot s (0,0,-5)
    (Char '=',ctl) -> mod_zoom s (if ctl then 0.05 else 0.01)
    (Char '-',ctl) -> mod_zoom s (if ctl then -0.05 else -0.01)
    (Char 'I',_) -> mod_init s
    (Char 'Q',_) -> exitWith ExitSuccess
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
  depthFunc $= Nothing
  lighting $= Disabled
  normalize $= Enabled

timer_f :: Timeout -> IO ()
timer_f dly = do
  postRedisplay Nothing
  addTimerCallback dly (timer_f dly)

gl_gr_obj :: GLsizei -> Timeout -> [FilePath] -> IO ()
gl_gr_obj sz dly fn = do
  ln <- gr_load_set fn
  _ <- initialize "GR-OBJ" []
  initialDisplayMode $= [RGBAMode,DoubleBuffered]
  initialWindowSize $= Size sz sz
  initialWindowPosition $= Position 0 0
  _ <- createWindow "GL"
  gl_init
  s <- newIORef state_0
  displayCallback $= withIORef s (gl_draw ln)
  keyboardMouseCallback $= Just (gl_keyboard s)
  addTimerCallback dly (timer_f dly)
  mainLoop

usg :: T.OPT_USG
usg = ["obj-gr [opt] file-name..."]

opt :: [T.OPT_USR]
opt =
  [("delay","100","int","timer delay (ms)")
  ,("size","400","int","window size (px)")]

main :: IO ()
main = do
  (o,a) <- T.opt_get_arg True usg opt
  case a of
    "obj-gr":fn -> gl_gr_obj (T.opt_read o "size") (T.opt_read o "delay") fn
    _ -> T.opt_usage usg opt

