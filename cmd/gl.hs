{-
GLU  = <https://www.khronos.org/registry/OpenGL/specs/gl/glu1.3.pdf>
GLUT = <https://www.opengl.org/resources/libraries/glut/glut-3.spec.pdf>
-}

import Control.Monad {- base -}
import Data.Fixed {- base -}
import Data.IORef {- base -}
import Data.List.Split {- base -}
import System.Exit {- base -}
import System.FilePath {- filepath -}
import Text.Printf {- base -}

import Graphics.UI.GLUT {- GLUT -}

import qualified Data.CG.Minus.Geometry.OFF as OFF {- hcg-minus -}
import Data.CG.Minus.Plain {- hcg-minus -}
import Data.CG.Minus.Projection {- hcg-minus -}

import qualified Music.Theory.Graph.OBJ as T {- hmt -}
import qualified Music.Theory.Graph.Type as T {- hmt -}
import qualified Music.Theory.Opt as T {- hmt -}

-- * GEOMETRY

type R = GLfloat

-- * GR/OBJ-OFF

type GR = T.LBL (V4 R) ()

-- | If OBJ file has no edges and if CH is true then make edges for all adjacent vertices.
--   If CH is true and there are edges, CH is ignored, allowing mixed sets to be loaded by setting CH.
obj_load :: Bool -> FilePath -> IO GR
obj_load ch fn = do
  (v,e) <- T.obj_load_v3_graph fn
  let f (i,(x,y,z)) = (i,(x,y,z,0))
  case (ch,null e) of
    (True,True) -> return (map f v,zip (map (\i -> (i,i + 1)) [0 .. length v - 2]) (repeat ()))
    (False,True) -> error "obj_load?"
    (_,False) -> return (map f v,e)

off_load :: FilePath -> IO GR
off_load =
  let f (x,y,z) = (x,y,z,0)
  in fmap OFF.off_graph . OFF.off_load_either (f,id)

obj_off_load :: Bool -> FilePath -> IO GR
obj_off_load ch fn =
  case takeExtension fn of
    ".obj" -> obj_load ch fn
    ".off" -> if ch then error "CH AT OFF?" else off_load fn
    _ -> error "obj_off_load: EXT?"

gr_to_vsq :: GR -> [V4 R]
gr_to_vsq (v,e) =
  let ix = maybe (error "?") id . flip lookup v
      f ((i,j),_) = [ix i,ix j]
  in concatMap f e

type LN = [[V4 R]]

gr_load_set :: (Bool,Bool) -> [FilePath] -> IO LN
gr_load_set (ch,nrm) fn = do
  v <- mapM (fmap gr_to_vsq . obj_off_load ch) fn
  let c = (if nrm then v4_normalise (-1,1) else id) (concat v)
  return (chunksOf 16 c)  -- does sending vertices in chunks help?

-- * IOREF

withIORef :: IORef a -> (a -> IO ()) -> IO ()
withIORef s f = readIORef s >>= f

-- * STATE

-- | (zoom,rotation-(x,y,z),translation-(x,y,z),osd,prj)
type State = (R,V3 R,V3 R,Bool,V4 R -> V3 R)

state_0 :: State
state_0 = (1.0,(20,30,0),(0,0,0),False,prj_xyz)

mod_osd :: IORef State -> IO ()
mod_osd s = modifyIORef s (\(z,t,r,o,p) -> (z,t,r,not o,p))

mod_trs_f :: V3 R -> State -> State
mod_trs_f (dx,dy,dz) (s,r,(x,y,z),o,p) = (s,r,(x + dx,y + dy,z + dz),o,p)

mod_trs :: IORef State -> V3 R -> IO ()
mod_trs s d = modifyIORef s (mod_trs_f d)

mod_rot_f :: V3 R -> State -> State
mod_rot_f (dx,dy,dz) (s,(x,y,z),t,o,p) =
  let f i j = Data.Fixed.mod' (i + j) 360
  in (s,(f x dx,f y dy,f z dz),t,o,p)

mod_rot :: IORef State -> V3 R -> IO ()
mod_rot s d = modifyIORef s (mod_rot_f d)

set_rot :: IORef State -> V3 R -> IO ()
set_rot s rt = modifyIORef s (\(sc,_,tr,o,p) -> (sc,rt,tr,o,p))

mod_zoom_f :: R -> State -> State
mod_zoom_f n (s,r,t,o,p) = (s + n,r,t,o,p)

mod_zoom :: IORef State -> R -> IO ()
mod_zoom s n = modifyIORef s (mod_zoom_f n)

set_init :: IORef State -> IO ()
set_init s = modifyIORef s (const state_0)

set_prj_f :: (V4 R -> V3 R) -> State -> State
set_prj_f p (s,r,t,o,_) = (s,r,t,o,p)

set_prj :: IORef State -> (V4 R -> V3 R) -> IO ()
set_prj s p = modifyIORef s (set_prj_f p)

-- * GL

{-
gl_with_time :: String -> IO x -> IO ()
gl_with_time m x = do
  t0 <- elapsedTime
  _ <- x
  t1 <- elapsedTime
  print (m,t1 - t0)
-}

gl_render_ln :: State -> LN -> IO ()
gl_render_ln (_,_,_,_,p) =
  let v (x,y,z) = Vertex3 x y z
      f i = renderPrimitive Lines (mapM_ (vertex . v . p) i)
  in mapM_ f

gl_grey :: Color4 R
gl_grey = Color4 0.5 0.5 0.5 0.5

gl_render_state :: State -> IO ()
gl_render_state (sc,(rx,ry,rz),(tx,ty,tz),_,_) = do
    translate (Vector3 tx ty tz)
    rotate rx (Vector3 1 0 0)
    rotate ry (Vector3 0 1 0)
    rotate rz (Vector3 0 0 1)
    scale sc sc sc
    color gl_grey

r_to_int :: R -> Int
r_to_int = round

state_pp :: State -> String
state_pp (sc,(rx,ry,rz),(tx,ty,tz),_,_) =
  let i n = r_to_int (if n > 180 then n - 360 else n)
  in printf "%.2f (%d,%d,%d) (%.1f,%.1f,%.1f)" sc (i rx) (i ry) (i rz) tx ty tz

gl_render_txt :: State -> IO ()
gl_render_txt st = do
  let (_,_,_,o,_) = st
  when o (rasterPos (Vertex3 (- 2.5) (-2.5) 0 :: Vertex3 R) >>
          renderString Fixed8By13 (state_pp st))

gl_draw :: LN -> State -> IO ()
gl_draw ln s = do
  clear [ColorBuffer]
  preservingMatrix (gl_render_txt s >> gl_render_state s >> gl_render_ln s ln)
  swapBuffers

gl_keydown :: IORef State -> Key -> Modifiers -> IO ()
gl_keydown s ky m = do
  let a = alt m == Down
      c = ctrl m == Down
      r = 5
  case ky of
    SpecialKey KeyDown -> if c then mod_trs s (0,-0.1,0) else mod_rot s (- r,0,0)
    SpecialKey KeyUp -> if c then mod_trs s (0,0.1,0) else mod_rot s (r,0,0)
    SpecialKey KeyLeft -> if c then mod_trs s (-0.1,0,0) else mod_rot s (0,- r,0)
    SpecialKey KeyRight -> if c then mod_trs s (0.1,0,0) else mod_rot s (0,r,0)
    SpecialKey KeyPageUp -> if c then mod_trs s (0,0,0.1) else mod_rot s (0,0,r)
    SpecialKey KeyPageDown -> if c then mod_trs s (0,0,-0.1) else mod_rot s (0,0,- r)
    SpecialKey KeyF1 -> set_prj s prj_xyz
    SpecialKey KeyF2 -> set_prj s prj_xyw
    SpecialKey KeyF3 -> set_prj s prj_xzw
    SpecialKey KeyF4 -> set_prj s prj_yzw
    Char '=' -> mod_zoom s (if c then 0.1 else 0.01)
    Char '-' -> mod_zoom s (if c then -0.1 else -0.01)
    Char '1' -> set_rot s (if not a then (0,0,0) else (0,180,0)) -- Y
    Char '2' -> set_rot s (if not a then (90,0,0) else (270,0,0)) -- X
    Char '3' -> set_rot s (if not a then (90,0,90) else (270,0,270)) -- X/Z
    Char '4' -> set_rot s (if not a then (0,0,90) else (0,0,270)) -- Z
    Char '5' -> set_rot s (if not a then (90,0,180) else (90,180,0)) -- Y/Z
    Char '0' -> set_init s
    Char 'o' -> mod_osd s
    Char 'Q' -> exitWith ExitSuccess
    _ -> return ()

gl_keyboard :: IORef State -> Key -> KeyState -> Modifiers -> Position -> IO ()
gl_keyboard s ky ks m _ = if ks == Down then gl_keydown s ky m else return ()

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

gl_gr_obj :: (Bool,Bool) -> GLsizei -> Timeout -> [FilePath] -> IO ()
gl_gr_obj opt sz dly fn = do
  ln <- gr_load_set opt fn
  _ <- initialize "GR-OBJ-OFF" []
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

cli_usg :: T.OPT_USG
cli_usg = ["obj-gr [opt] file-name..."]

cli_opt :: [T.OPT_USR]
cli_opt =
  [("chain","False","bool","OBJ is vertex sequence")
  ,("delay","100","int","timer delay (ms)")
  ,("normalise","False","bool","normalise vertex data to (-1,1)")
  ,("size","400","int","window size (px)")]

main :: IO ()
main = do
  (o,a) <- T.opt_get_arg True cli_usg cli_opt
  case a of
    "obj-gr":fn -> gl_gr_obj
                   (T.opt_read o "chain",T.opt_read o "normalise")
                   (T.opt_read o "size")
                   (T.opt_read o "delay")
                   fn
    _ -> T.opt_usage cli_usg cli_opt

