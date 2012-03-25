{- ============================================================================
| Copyright 2010 Matthew D. Steele <mdsteele@alum.mit.edu>                    |
|                                                                             |
| This file is part of Pylos.                                                 |
|                                                                             |
| Pylos is free software: you can redistribute it and/or modify it            |
| under the terms of the GNU General Public License as published by the Free  |
| Software Foundation, either version 3 of the License, or (at your option)   |
| any later version.                                                          |
|                                                                             |
| Pylos is distributed in the hope that it will be useful, but                |
| WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  |
| or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License    |
| for more details.                                                           |
|                                                                             |
| You should have received a copy of the GNU General Public License along     |
| with Pylos.  If not, see <http://www.gnu.org/licenses/>.                    |
============================================================================ -}

{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Pylos.Draw
  (initializeScreen,
   -- * Sprites
   Sprite, spriteWidth, spriteHeight, spriteSize, subSprite,
   -- * The Draw monad
   Draw, MonadDraw(..), debugDraw,
   -- * The Handler monad
   Handler, MonadHandler(..), canvasRect, runHandlerIO,
   getRelativeMousePos, withInputsSuppressed,
   -- * The Paint monad
   Paint, drawToScreen,
   -- * Reference cells
   DrawRef, newDrawRef, readDrawRef, writeDrawRef,
   -- * Drawing onto canvases
   -- ** Blitting sprites
   blitTopleft, blitLoc, blitStretch, blitRepeat,
   -- ** Geometric primitives
   drawOval, drawPolygon, tintOval,
   -- * Fonts and text
   Font, drawText, renderText, textSize, textWidth,
   -- * Loading resources
   loadFont, loadSprite)
where

import Control.Applicative ((<*), (<*>), Applicative, pure)
import Control.Arrow ((&&&), (***))
import Control.Monad (when)
import Data.Bits (xor)
import qualified Data.HashTable as HT
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8, Word32)
import Foreign (withForeignPtr)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Extras as SDLx
import qualified Graphics.UI.SDL.Image as SDLi
import qualified Graphics.UI.SDL.TTF as SDLt
import qualified System.FilePath as FilePath (combine)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak (addFinalizer)

import System.MacOSX.Bundle (getResourcePath)

import Pylos.Constants (screenHeight, screenRect, screenWidth)
import Pylos.Data.Color (Color(Color), Tint(Tint))
import Pylos.Data.Point
import Pylos.Utility (delayFinalizers, untilM)

-------------------------------------------------------------------------------
-- Setting up the screen:

initializeScreen :: Bool -> IO ()
initializeScreen fullscreen = do
  SDL.glSetAttribute SDL.glDoubleBuffer 1
  SDLx.glSetVsyncEnabled True
  _ <- SDL.setVideoMode screenWidth screenHeight 32 $ SDL.OpenGL :
                        (if fullscreen then [SDL.Fullscreen] else [])
  -- Turn off the depth buffer:
  GL.depthFunc $= Nothing
  GL.depthMask $= GL.Disabled
  -- Set up blending:
  GL.blend $= GL.Enabled
  GL.blendEquation $= GL.FuncAdd
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  -- Turn on antialiasing:
  GL.lineSmooth $= GL.Enabled
  GL.pointSmooth $= GL.Enabled
  GL.polygonSmooth $= GL.Enabled
  -- Enable textures:
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureFunction $= GL.Modulate
  -- Set the view:
  GL.clearColor $= GL.Color4 0 0 0 0
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral screenWidth)
                                           (fromIntegral screenHeight))
  GL.ortho 0 (fromIntegral screenWidth) (fromIntegral screenHeight) 0 (-1) 1
  -- We are drawing "upside-down" relative to how GL normally draws things
  -- (i.e. we have y increasing downwards rather than upwards), so we set the
  -- pixelZoom such that when we use something like GL.drawPixels, the raster
  -- position will correspond to the top-left of the pixel array rather than
  -- the bottom left.
  GL.pixelZoom $=  (1, -1)

-------------------------------------------------------------------------------
-- Sprites:

data Sprite = Sprite
  { spriteTexture :: !GL.TextureObject,
    -- | Return the width of the 'Sprite', in pixels.
    spriteWidth :: !Int,
    -- | Return the height of the 'Sprite', in pixels.
    spriteHeight :: !Int }

-- | Return the width and height of the 'Sprite', in pixels.
spriteSize :: Sprite -> (Int, Int)
spriteSize = spriteWidth &&& spriteHeight

subSprite :: (MonadDraw m) => Sprite -> IRect -> m Sprite
subSprite sprite rect = newSprite (rectSize rect) $ do
  blitTopleft sprite $ pNeg $ rectTopleft rect

-------------------------------------------------------------------------------
-- The Draw monad:

newtype Draw a = Draw { fromDraw :: IO a }
  deriving (Applicative, Functor, Monad)

class (Applicative m, Monad m) => MonadDraw m where
  runDraw :: Draw a -> m a

instance MonadDraw Draw where runDraw = id
instance MonadDraw IO where runDraw = fromDraw

debugDraw :: (MonadDraw m) => String -> m ()
debugDraw = drawIO . putStrLn

-------------------------------------------------------------------------------
-- The Handler monad:

newtype Handler a = Handler { fromHandler :: IRect -> IO a }

instance Functor Handler where
  fmap fn (Handler hfn) = Handler (fmap fn . hfn)

instance Applicative Handler where
  pure = Handler . const . pure
  (Handler hfn1) <*> (Handler hfn2) =
    Handler $ \rect -> hfn1 rect <*> hfn2 rect

instance Monad Handler where
  return = Handler . const . return
  (Handler hfn) >>= fn = Handler $ \rect -> do
    x <- hfn rect
    fromHandler (fn x) rect
  fail = Handler . const . fail

instance MonadDraw Handler where
  runDraw = Handler . const . fromDraw

class (MonadDraw m) => MonadHandler m where
  runHandler :: Handler a -> m a

  canvasSize :: m (Int, Int)

  canvasWidth :: m Int
  canvasWidth = fmap fst canvasSize

  canvasHeight :: m Int
  canvasHeight = fmap snd canvasSize

  withSubCanvas :: IRect -> m a -> m a

instance MonadHandler Handler where
  runHandler = id
  canvasSize = Handler (return . rectSize)
  canvasWidth = Handler (return . rectW)
  canvasHeight = Handler (return . rectH)
  withSubCanvas subrect handler = Handler $ \rect -> do
    fromHandler handler (subrect `rectPlus` rectTopleft rect)

canvasRect :: (MonadHandler m) => m IRect
canvasRect = do
  (width, height) <- canvasSize
  return $ Rect 0 0 width height

runHandlerIO :: Handler a -> IRect -> IO a
runHandlerIO = fromHandler

getRelativeMousePos :: (MonadHandler m) => m (Maybe IPoint)
getRelativeMousePos = runHandler $ Handler $ \rect -> do
  suppressed <- readIORef inputsSuppressed
  if suppressed then return Nothing else do
  (absoluteMouseX, absoluteMouseY, _) <- SDL.getMouseState
  return $ Just $ Point (absoluteMouseX - rectX rect)
                        (absoluteMouseY - rectY rect)

withInputsSuppressed :: (MonadHandler m) => m a -> m a
withInputsSuppressed action = do
  old <- drawIO (readIORef inputsSuppressed <*
                 writeIORef inputsSuppressed True)
  action <* drawIO (writeIORef inputsSuppressed old)

{-# NOINLINE inputsSuppressed #-} -- needed for unsafePerformIO
inputsSuppressed :: IORef Bool
inputsSuppressed = unsafePerformIO (newIORef False)

-------------------------------------------------------------------------------
-- The Paint monad:

newtype Paint a = Paint { fromPaint :: IO a }
  deriving (Applicative, Functor, Monad)

instance MonadDraw Paint where runDraw = Paint . fromDraw

instance MonadHandler Paint where
  runHandler handler = Paint $ do
    scissor <- GL.get GL.scissor
    fromHandler handler (fromScissor scissor)
  canvasSize = Paint $ do
    scissor <- GL.get GL.scissor
    return $ case scissor of
      Nothing -> (screenWidth, screenHeight)
      Just (_, GL.Size w h) -> (fromIntegral w, fromIntegral h)
  withSubCanvas = paintWithSubCanvas

paintWithSubCanvas :: IRect -> Paint a -> Paint a
paintWithSubCanvas rect paint = Paint $ GL.preservingMatrix $ do
  GL.translate $ GL.Vector3 (toGLdouble $ rectX rect)
                            (toGLdouble $ rectY rect) 0
  oldScissor <- GL.get GL.scissor
  let oldRect = fromScissor oldScissor
  let rect' = oldRect `rectIntersection` (rect `rectPlus` rectTopleft oldRect)
  GL.scissor $= Just
      (GL.Position (fromIntegral $ rectX rect')
                   (fromIntegral $ screenHeight - rectY rect' - rectH rect'),
       GL.Size (fromIntegral $ rectW rect') (fromIntegral $ rectH rect'))
  result <- fromPaint paint
  GL.scissor $= oldScissor
  return result

drawToScreen :: Paint () -> IO ()
drawToScreen paint = do
  GL.clear [GL.ColorBuffer]
  fromPaint paint
  GL.flush -- TODO: Are the flush and finish at all necessary?
  GL.finish
  SDL.glSwapBuffers

-------------------------------------------------------------------------------
-- Reference cells:

newtype DrawRef a = DrawRef (IORef a)

newDrawRef :: (MonadDraw m) => a -> m (DrawRef a)
newDrawRef = runDraw . Draw . fmap DrawRef . newIORef

readDrawRef :: (MonadDraw m) => DrawRef a -> m a
readDrawRef (DrawRef ref) = runDraw $ Draw $ readIORef ref

writeDrawRef :: (MonadDraw m) => DrawRef a -> a -> m ()
writeDrawRef (DrawRef ref) value = runDraw $ Draw $ writeIORef ref value

-------------------------------------------------------------------------------
-- Creating new sprites:

newSprite :: (MonadDraw m) => (Int, Int) -> Paint () -> m Sprite
newSprite (width, height) paint = runDraw $ Draw $ do
  oldBuffer <- GL.get GL.drawBuffer
  let newBuffer = case oldBuffer of GL.AuxBuffer i -> GL.AuxBuffer (i + 1)
                                    _ -> GL.AuxBuffer 0
  GL.drawBuffer $= newBuffer
  GL.clear [GL.ColorBuffer]
  oldScissor <- GL.get GL.scissor
  GL.scissor $= Just (GL.Position 0 0,
                      GL.Size (fromIntegral width) (fromIntegral height))
  GL.preservingMatrix $ do
    -- See note [New Sprite] below for why we do this little dance here.
    GL.scale 1 (-1) (1 :: GL.GLdouble)
    GL.translate $ GL.Vector3 0 (negate $ toGLdouble screenHeight) 0
    oldZoom <- GL.get GL.pixelZoom
    GL.pixelZoom $= (1, 1)
    fromPaint paint
    GL.pixelZoom $= oldZoom
  GL.scissor $= oldScissor
  makeSprite width height $ do
    GL.readBuffer $= newBuffer
    GL.copyTexImage2D Nothing 0 GL.RGB' (GL.Position 0 0)
        (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) 0
    GL.drawBuffer $= oldBuffer

-- Note [New Sprite]:
--   Unfortunately, GL.copyTexImage2D doesn't seem to let us specify that the
--   start position is the top-left rather than the bottom-right.  So, we need
--   to carefully scale and translate the screen so that we draw everything
--   upside-down, and then when we do the GL.copyTexImage2D, we pretend that
--   the start position really is the top-left, and everything comes out fine.
--   However, since we're drawing everything upside-down, we also need to
--   temporarily set the pixelZoom to (1, 1) rather than (1, -1), so that if we
--   call GL.drawPixels the rastered pixels will come out the right way.

-------------------------------------------------------------------------------
-- Blitting sprites:

blitTopleft :: (Axis a) => Sprite -> Point a -> Paint ()
blitTopleft sprite (Point x y) =
  blitStretch sprite (Rect x y (fromIntegral $ spriteWidth sprite)
                               (fromIntegral $ spriteHeight sprite))

blitLoc :: (Axis a) => Sprite -> LocSpec a -> Paint ()
blitLoc sprite loc = blitTopleft sprite $ locTopleft loc $
                     (fromIntegral *** fromIntegral) $ spriteSize sprite

blitStretch :: (Axis a) => Sprite -> Rect a -> Paint ()
blitStretch sprite rect =
  blitGeneralized sprite whiteTint (fmap toGLdouble rect) (Rect 0 0 1 1)

blitRepeat :: (Axis a) => Sprite -> Point a -> Rect a -> Paint ()
blitRepeat sprite offset rect =
  let width = toGLdouble (spriteWidth sprite)
      height = toGLdouble (spriteHeight sprite)
      (Point ox oy) = fmap toGLdouble offset
      toRect = fmap toGLdouble rect
      texRect = Rect (negate ox / width) (negate oy / height)
                     (rectW toRect / width) (rectH toRect / height)
  in blitGeneralized sprite whiteTint toRect texRect

blitGeneralized :: Sprite -> Tint -> Rect GL.GLdouble -> Rect GL.GLdouble
                -> Paint ()
blitGeneralized sprite tint (Rect rx ry rw rh) (Rect tx ty tw th) = do
  Paint $ delayFinalizers sprite $ do
    GL.textureBinding GL.Texture2D $= Just (spriteTexture sprite)
    setTint tint
    GL.renderPrimitive GL.Quads $ do
      GL.texCoord $ GL.TexCoord2 tx ty
      glVertex rx ry
      GL.texCoord $ GL.TexCoord2 (tx + tw) ty
      glVertex (rx + rw) ry
      GL.texCoord $ GL.TexCoord2 (tx + tw) (ty + th)
      glVertex (rx + rw) (ry + rh)
      GL.texCoord $ GL.TexCoord2 tx (ty + th)
      glVertex rx (ry + rh)

-------------------------------------------------------------------------------
-- Geometric primitives:

-- | Draw an antialiased oval onto the canvas.
drawOval :: (Axis a) => Tint -> Rect a -> Paint ()
drawOval = strokeOval GL.LineLoop

tintOval :: (Axis a) => Tint -> Rect a -> Paint ()
tintOval = strokeOval GL.Polygon

strokeOval :: (Axis a) => GL.PrimitiveMode -> Tint -> Rect a -> Paint ()
strokeOval mode tint (Rect x y w h) = Paint $ when (w > 0 && h > 0) $ do
  let hRad = toGLdouble w / 2
      vRad = toGLdouble h / 2
  let cx = toGLdouble x + hRad
      cy = toGLdouble y + vRad
  GL.preservingMatrix $ do
    GL.translate $ GL.Vector3 cx cy 0
    let thetaStep = 2 / max hRad vRad
    drawPrimitive mode tint $ do
      untilM 0 (>= 2 * pi) (+thetaStep) $ \theta -> do
        GL.vertex $ GL.Vertex3 (hRad * cos theta) (vRad * sin theta) 0

drawPolygon :: (Axis a) => Tint -> [Point a] -> Paint ()
drawPolygon tint points = Paint $ do
  drawPrimitive GL.LineLoop tint $ mapM_ pointVertex' points

-------------------------------------------------------------------------------
-- Fonts and text:

newtype Font = Font SDLt.Font

-- | Draw text with the given font and color onto the screen at the specified
-- location.
drawText :: (Axis a) => Font -> Color -> LocSpec a -> String -> Paint ()
drawText font color spec string = Paint $ do
  surface <- renderText' font color string
  (format, _) <- surfaceFormats surface
  let width = SDL.surfaceGetWidth surface
      height = SDL.surfaceGetHeight surface
  let (Point x y) = locTopleft spec (fromIntegral width, fromIntegral height)
  GL.rasterPos (GL.Vertex3 (toGLdouble x) (toGLdouble y) 0)
  GL.textureBinding GL.Texture2D $= Nothing
  pixelsPtr <- SDL.surfaceGetPixels surface
  GL.drawPixels (GL.Size (fromIntegral width) (fromIntegral height))
                (GL.PixelData format GL.UnsignedByte pixelsPtr)

renderText :: (MonadDraw m) => Font -> Color -> String -> m Sprite
renderText font color string = runDraw $ Draw $ do
  renderText' font color string >>= makeSpriteFromSurface

renderText' :: Font -> Color -> String -> IO SDL.Surface
renderText' (Font font) color string =
  if null string then SDL.createRGBSurfaceEndian [SDL.SWSurface] 0 0 32
  else SDLt.renderTextBlended font string (toSDLColor color)

-- | Determine the width and height that a given string would have when
-- rendered in the given font.
textSize :: (MonadDraw m) => Font -> String -> m (Int, Int)
textSize (Font font) str = runDraw $ Draw $ SDLt.textSize font str

textWidth :: (MonadDraw m) => Font -> String -> m Int
textWidth = (fmap fst .) . textSize

-------------------------------------------------------------------------------
-- Loading resources:

{-# NOINLINE fontCache #-} -- needed for unsafePerformIO
fontCache :: HT.HashTable (FilePath, Int) Font
fontCache = unsafePerformIO $ HT.new (==) hash
  where hash (str, int) = HT.hashString str `xor` HT.hashInt int

loadFont :: (MonadDraw m) => FilePath -> Int -> m Font
loadFont name size = runDraw $ Draw $ do
  let key = (name, size)
  mbFont <- HT.lookup fontCache key
  case mbFont of
    Just font -> return font
    Nothing -> do
      mbPath <- getResourcePath (FilePath.combine "fonts" name)
      case mbPath of
        Nothing -> fail "loadFont: getResourcePath failed"
        Just path -> do
          font <- fmap Font $ SDLt.openFont path size
          HT.insert fontCache key font
          return font

loadSprite :: (MonadDraw m) => FilePath -> m Sprite
loadSprite name = runDraw $ Draw $ do
  mbPath <- getResourcePath (FilePath.combine "images" name)
  case mbPath of
    Nothing -> fail "loadSprite: getResourcePath failed"
    Just path -> SDLi.load path >>= makeSpriteFromSurface

-------------------------------------------------------------------------------
-- Private utility functions:

makeSprite :: Int -> Int -> IO () -> IO Sprite
makeSprite width height action = do
  [texName] <- GL.genObjectNames 1
  GL.textureBinding GL.Texture2D $= Just texName
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  () <- action
  let sprite = Sprite { spriteTexture = texName,
                        spriteWidth = width,
                        spriteHeight = height }
  addFinalizer sprite $ GL.deleteObjectNames [texName]
  return sprite

makeSpriteFromSurface :: SDL.Surface -> IO Sprite
makeSpriteFromSurface surface = do
  (format, format') <- surfaceFormats surface
  let width = SDL.surfaceGetWidth surface
      height = SDL.surfaceGetHeight surface
  makeSprite width height $ do
    withForeignPtr surface $ const $ do
      pixelsPtr <- SDL.surfaceGetPixels surface
      GL.texImage2D Nothing GL.NoProxy 0 format'
          (GL.TextureSize2D (fromIntegral width) (fromIntegral height))
          0 (GL.PixelData format GL.UnsignedByte pixelsPtr)

-- | Determine the appropriate OpenGL pixel formats to use when interpreting
-- the raw pixel data of the given SDL surface.
surfaceFormats :: SDL.Surface -> IO (GL.PixelFormat, GL.PixelInternalFormat)
surfaceFormats surface = do
  let pixelFormat = SDL.surfaceGetPixelFormat surface
  bmask <- SDLx.pixelFormatGetBmask pixelFormat
  let bgr = bmask == ntohl 0xff000000 -- Are we in BGR order or RGB order?
  numColors <- SDL.pixelFormatGetBytesPerPixel pixelFormat
  case numColors of
    4 -> return (if bgr then GL.BGRA else GL.RGBA, GL.RGBA')
    3 -> return (if bgr then GL.BGR  else GL.RGB,  GL.RGB')
    _ -> fail ("numColors = " ++ show numColors)

-- | Convert a big-endian word to native endianness.
foreign import ccall unsafe "netinet/in.h" ntohl :: Word32 -> Word32

fromScissor :: Maybe (GL.Position, GL.Size) -> IRect
fromScissor = maybe screenRect $ \(GL.Position x y, GL.Size w h) ->
  Rect (fromIntegral x) (screenHeight - fromIntegral y - fromIntegral h)
       (fromIntegral w) (fromIntegral h)

setTint :: Tint -> IO ()
setTint (Tint r g b a) = GL.color $
  GL.Color4 (fromWord r) (fromWord g) (fromWord b) (fromWord a)

fromWord :: Word8 -> GL.GLdouble
fromWord = (* recip 255) . fromIntegral

glVertex :: GL.GLdouble -> GL.GLdouble -> IO ()
glVertex x y = GL.vertex $ GL.Vertex3 x y 0

pointVertex' :: (Axis a) => Point a -> IO ()
pointVertex' (Point x y) =
  GL.vertex $ GL.Vertex3 (toGLdouble x + 0.5) (toGLdouble y + 0.5) 0

toSDLColor :: Color -> SDL.Color
toSDLColor (Color r g b) = SDL.Color r g b

drawPrimitive :: GL.PrimitiveMode -> Tint -> IO () -> IO ()
drawPrimitive mode tint action = do
  GL.textureBinding GL.Texture2D $= Nothing
  setTint tint
  GL.renderPrimitive mode action

toGLdouble :: (Axis a) => a -> GL.GLdouble
toGLdouble = toFloating

whiteTint :: Tint
whiteTint = Tint 255 255 255 255

drawIO :: (MonadDraw m) => IO a -> m a
drawIO = runDraw . Draw

-------------------------------------------------------------------------------
