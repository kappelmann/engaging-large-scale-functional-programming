{-# LANGUAGE FlexibleContexts #-}
module Display where

import Turtle
import Exercise10 (execute, Rule(..), LSystem(..), update)

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)
import Data.IORef (newIORef)
import Data.List ( map, null, take, elemIndices )

import Data.Word (Word8)
import Data.Vector.Storable

import Foreign.Ptr ()
import Foreign.ForeignPtr

import Codec.Picture.Types
import Codec.Picture.Png (writePng)
import Codec.Picture.Gif ()

import System.Directory (createDirectoryIfMissing)


main :: IO ()
main = display (LSystem "F" ['F' :->: "F+++F---F---F+++F"]) 3

display :: LSystem -> Integer -> IO ()
display ils iN = do 
  lsRef <- newIORef ils 
  itsRef <- newIORef iN 
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= pointToSize theCanvas
  getArgsAndInitialize
  w <- createWindow "Turtle Graphics"
  displayCallback $= drawRefs lsRef itsRef
  reshapeCallback $= Just (\x -> viewport $= (Position 0 0, x))
  actionOnWindowClose $= MainLoopReturns
  idleCallback $= Just (idleCb lsRef itsRef)
  specialCallback $= Just (arrowKeyCb lsRef itsRef)
  drawRefs lsRef itsRef
  mainLoop
  where
    idleCb lsRef itsRef = do
      ls <- get lsRef
      ls' <- update ls
      if ls == ls'
        then return ()
        else lsRef $= ls' >> drawRefs lsRef itsRef

    arrowKeyCb lsRef itsRef key _ = do
      its <- get itsRef
      let its' = max 0 $ case key of
                           KeyUp -> its + 1
                           KeyDown -> its - 1
                           _ -> its
      if its == its'
         then return ()
         else itsRef $= its' >> drawRefs lsRef itsRef

    drawRefs lsRef itsRef = do
      ls <- get lsRef
      its <- get itsRef
      draw ls its 

penToRGB :: Pen -> GL.Color3 GL.GLfloat
penToRGB (Colour r g b)  =  GL.Color3 r g b
penToRGB Inkless  =  error "penToRGB: inkless"

pointToSize :: Pnt -> GL.Size
pointToSize (Pnt x y) = GL.Size (ceiling x) (ceiling y)

sizeToPoint :: GL.Size -> Pnt
sizeToPoint (GL.Size x y) = Pnt (fromIntegral x) (fromIntegral y)

draw :: LSystem -> Integer -> IO ()
draw c n = do clear [ColorBuffer]
              loadIdentity
              background 
              toGraphic $ rescale $ execute c n
              swapBuffers

toGraphic :: [Ln] -> IO ()
toGraphic  = Prelude.mapM_ f
  where
  f (Ln pen startP endP)  =
    GL.color (penToRGB pen) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)

background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ Prelude.mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]


toVertex :: Pnt -> IO ()
toVertex (Pnt x y)  =  GL.vertex $ GL.Vertex3 
 (realToFrac x) (realToFrac y) (0::GL.GLfloat)



 -- Rescales all points in a list of lines
--  from an arbitrary scale
--  to (-1.-1) - (1.1)

rescale :: [Ln] -> [Ln]
rescale lines | Data.List.null points = []
              | otherwise    = Data.List.map f lines
  where
  f (Ln pen p q)  =  Ln pen (g p) (g q)
  g p             =  swap ((p - p0) / s)
  points          =  [ r | Ln pen p q <- lines, r <- [p, q] ]
  hi              =  Prelude.foldr1 lub points
  lo              =  Prelude.foldr1 glb points
  s               =  scalarMax (hi - lo) * scalar 0.55
  p0              =  (hi + lo) * scalar 0.5
  swap (Pnt x y)  =  Pnt y x



-- Window parameters

theCanvas :: Pnt
theCanvas  =  Pnt 800 800

theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = penToRGB white

-- adaptation of main function that saves the n first iterations as png image

savePngs :: LSystem -> Integer -> String -> IO ()
savePngs c n s = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= pointToSize theCanvas
  getArgsAndInitialize
  w <- createWindow "Turtle Graphics"
  displayCallback $= draw c n
  reshapeCallback $= Just (\x -> viewport $= (Position 0 0, x))
  actionOnWindowClose $= MainLoopReturns
  prepareDirectory s
  drawLoop c n w s
  mainLoop
  
drawLoop :: LSystem -> Integer -> Window -> String -> IO ()
drawLoop c n w s
  | n == 0 = return ()
  | otherwise = do drawAndSave c n w $ s Prelude.++ show n Prelude.++ ".png"
                   drawLoop c (n-1) w s

drawAndSave :: LSystem -> Integer -> Window -> String -> IO ()
drawAndSave c n w s = do
  clear [ColorBuffer]
  loadIdentity
  background 
  toGraphic $ rescale $ execute c n
  saveImage w s
  swapBuffers

prepareDirectory :: String -> IO ()
prepareDirectory s = do
  let is = Data.List.elemIndices '/' s
  if Data.List.null is
  then return ()
  else createDirectoryIfMissing True $ Data.List.take (Prelude.last is) s

-- read pixels, convert them to first a Vector then a JuicyPixel image
-- and save the image as a PNG file
-- adapted from https://www.reddit.com/r/haskell/comments/dee0iz/converting_opengl_window_to_a_png_image_in_haskell/
saveImage :: Window -> String -> IO ()
saveImage window name = do
  currentWindow $= Just window
  Size w h <- get windowSize
  let npixels = fromIntegral (w*h) :: Int
      nbytes  = 3*npixels
  fptr <- mallocForeignPtrArray nbytes :: IO (ForeignPtr Word8)
  withForeignPtr fptr $ \ptr -> do
    let pdata = PixelData RGB UnsignedByte ptr :: PixelData Word8
    readPixels (Position 0 0) (Size w h) pdata
  let fptr' = castForeignPtr fptr :: ForeignPtr (PixelBaseComponent PixelRGB8)
  let imgdata = unsafeFromForeignPtr0 fptr' npixels :: Vector (PixelBaseComponent PixelRGB8)
  let image = Image (fromIntegral w) (fromIntegral h) imgdata :: Image PixelRGB8
  writePng name image

