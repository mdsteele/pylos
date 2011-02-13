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

{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Pylos.Draw
  (initializeScreen,
   -- * Sprites
   Sprite, spriteWidth, spriteHeight, spriteSize, subSprite,
   -- * The DrawOn Monad
   DrawOn, runDraw, debugDraw,
   -- * Reference cells
   DrawRef, newDrawRef, readDrawRef, writeDrawRef,
   -- * The Draw Monad
   Draw, drawToScreen,
   Canvas, canvasWidth, canvasHeight, canvasSize, canvasRect,
   -- * Drawing onto canvases
   -- ** Blitting sprites
   blitTopleft, blitLoc, blitStretch, blitRepeat,
   -- ** Geometric primitives
   drawOval, drawPolygon, tintOval,
   -- ** Miscellaneous
   withSubCanvas,
   -- * Fonts and text
   Font, drawText, renderText, textSize, textWidth,
   -- * Loading resources
   loadFont, loadSprite)
where

import Control.Applicative (Applicative(pure, (<*>)))
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

subSprite :: Sprite -> IRect -> DrawOn z Sprite
subSprite sprite rect = newSprite (rectSize rect) $ do
  blitTopleft sprite $ pNeg $ rectTopleft rect

-------------------------------------------------------------------------------
-- The DrawOn monad:

newtype DrawOn a b = DrawOn { fromDrawOn :: IO b }

instance Applicative (DrawOn a) where
  pure = DrawOn . pure
  (DrawOn f) <*> (DrawOn g) = DrawOn (f <*> g)

instance Functor (DrawOn a) where
  fmap fn m = DrawOn (fmap fn (fromDrawOn m))

instance Monad (DrawOn a) where
  return = DrawOn . return
  draw >>= fn = DrawOn $ do b <- fromDrawOn draw
                            fromDrawOn (fn b)
  fail = DrawOn . fail

runDraw :: DrawOn () a -> IO a
runDraw = fromDrawOn

debugDraw :: String -> DrawOn a ()
debugDraw = DrawOn . putStrLn

-------------------------------------------------------------------------------
-- The Draw monad:

data Canvas

type Draw a = DrawOn Canvas a

drawToScreen :: Draw () -> IO ()
drawToScreen draw = do
  GL.clear [GL.ColorBuffer]
  fromDrawOn draw
  GL.flush -- TODO: Are the flush and finish at all necessary?
  GL.finish
  SDL.glSwapBuffers

canvasWidth :: Draw Int
canvasWidth = fmap fst canvasSize

canvasHeight :: Draw Int
canvasHeight = fmap snd canvasSize

canvasSize :: Draw (Int, Int)
canvasSize = DrawOn $ do
  scissor <- GL.get GL.scissor
  return $ case scissor of
    Nothing -> (screenWidth, screenHeight)
    Just (_, GL.Size w h) -> (fromIntegral w, fromIntegral h)

canvasRect :: Draw IRect
canvasRect = do
  (width, height) <- canvasSize
  return $ Rect 0 0 width height

-------------------------------------------------------------------------------
-- Reference cells:

newtype DrawRef a = DrawRef (IORef a)

newDrawRef :: b -> DrawOn a (DrawRef b)
newDrawRef = DrawOn . fmap DrawRef . newIORef

readDrawRef :: DrawRef b -> DrawOn a b
readDrawRef (DrawRef ref) = DrawOn (readIORef ref)

writeDrawRef :: DrawRef b -> b -> DrawOn a ()
writeDrawRef (DrawRef ref) value = DrawOn (writeIORef ref value)

-------------------------------------------------------------------------------
-- Creating new sprites:

newSprite :: (Int, Int) -> Draw () -> DrawOn a Sprite
newSprite (width, height) draw = DrawOn $ do
  oldBuffer <- GL.get GL.drawBuffer
  let newBuffer = case oldBuffer of GL.AuxBuffer i -> GL.AuxBuffer (i + 1)
                                    _ -> GL.AuxBuffer 0
  GL.drawBuffer $= newBuffer
  GL.clear [GL.ColorBuffer]
  oldScissor <- GL.get GL.scissor
  GL.scissor $= Just (GL.Position 0 0,
                      GL.Size (fromIntegral width) (fromIntegral height))
  GL.preservingMatrix $ do
    GL.scale 1 (-1) (1 :: GL.GLdouble)
    GL.translate $ GL.Vector3 0 (negate $ toGLdouble screenHeight) 0
    fromDrawOn draw
  GL.scissor $= oldScissor
  makeSprite width height $ do
    GL.readBuffer $= newBuffer
    GL.copyTexImage2D Nothing 0 GL.RGB' (GL.Position 0 0)
        (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) 0
    GL.drawBuffer $= oldBuffer

-------------------------------------------------------------------------------
-- Blitting sprites:

blitTopleft :: (Axis a) => Sprite -> Point a -> Draw ()
blitTopleft sprite (Point x y) =
  blitStretch sprite (Rect x y (fromIntegral $ spriteWidth sprite)
                               (fromIntegral $ spriteHeight sprite))

blitLoc :: (Axis a) => Sprite -> LocSpec a -> Draw ()
blitLoc sprite loc = blitTopleft sprite $ locTopleft loc $
                     (fromIntegral *** fromIntegral) $ spriteSize sprite

blitStretch :: (Axis a) => Sprite -> Rect a -> Draw ()
blitStretch sprite rect =
  blitGeneralized sprite whiteTint (fmap toGLdouble rect) (Rect 0 0 1 1)

blitRepeat :: (Axis a) => Sprite -> Point a -> Rect a -> Draw ()
blitRepeat sprite offset rect =
  let width = toGLdouble (spriteWidth sprite)
      height = toGLdouble (spriteHeight sprite)
      (Point ox oy) = fmap toGLdouble offset
      toRect = fmap toGLdouble rect
      texRect = Rect (negate ox / width) (negate oy / height)
                     (rectW toRect / width) (rectH toRect / height)
  in blitGeneralized sprite whiteTint toRect texRect

blitGeneralized :: Sprite -> Tint -> Rect GL.GLdouble -> Rect GL.GLdouble
                -> Draw ()
blitGeneralized sprite tint (Rect rx ry rw rh) (Rect tx ty tw th) = do
  DrawOn $ delayFinalizers sprite $ do
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
drawOval :: (Axis a) => Tint -> Rect a -> Draw ()
drawOval = strokeOval GL.LineLoop

tintOval :: (Axis a) => Tint -> Rect a -> Draw ()
tintOval = strokeOval GL.Polygon

strokeOval :: (Axis a) => GL.PrimitiveMode -> Tint -> Rect a -> Draw ()
strokeOval mode tint (Rect x y w h) = DrawOn $ when (w > 0 && h > 0) $ do
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

drawPolygon :: (Axis a) => Tint -> [Point a] -> Draw ()
drawPolygon tint points = DrawOn $ do
  drawPrimitive GL.LineLoop tint $ mapM_ pointVertex' points

-------------------------------------------------------------------------------
-- Miscellaneous canvas functions:

withSubCanvas :: IRect -> Draw a -> Draw a
withSubCanvas rect draw = DrawOn $ GL.preservingMatrix $ do
  GL.translate $ GL.Vector3 (toGLdouble $ rectX rect)
                            (toGLdouble $ rectY rect) 0
  oldScissor <- GL.get GL.scissor
  let fromScissor (GL.Position x y, GL.Size w h) =
        Rect (fromIntegral x) (screenHeight - fromIntegral y - fromIntegral h)
             (fromIntegral w) (fromIntegral h)
  let oldRect = maybe screenRect fromScissor oldScissor
  let rect' = oldRect `rectIntersection` (rect `rectPlus` rectTopleft oldRect)
  GL.scissor $= Just
      (GL.Position (fromIntegral $ rectX rect')
                   (fromIntegral $ screenHeight - rectY rect' - rectH rect'),
       GL.Size (fromIntegral $ rectW rect') (fromIntegral $ rectH rect'))
  result <- fromDrawOn draw
  GL.scissor $= oldScissor
  return result

-------------------------------------------------------------------------------
-- Fonts and text:

newtype Font = Font SDLt.Font

drawText :: (Axis a) => Font -> Color -> LocSpec a -> String -> Draw ()
drawText font color spec string = do
  -- TODO: Can we blit the SDL surface without allocating a GL texture?
  sprite <- renderText font color string
  blitLoc sprite spec

renderText :: Font -> Color -> String -> DrawOn a Sprite
renderText (Font font) color string = DrawOn $ do
  surf <- if null string then SDL.createRGBSurfaceEndian [SDL.SWSurface] 0 0 32
          else SDLt.renderTextBlended font string (toSDLColor color)
  makeSpriteFromSurface surf

textSize :: Font -> String -> DrawOn a (Int, Int)
textSize (Font font) str = DrawOn $ SDLt.textSize font str

textWidth :: Font -> String -> DrawOn a Int
textWidth = (fmap fst .) . textSize

-------------------------------------------------------------------------------
-- Loading resources:

{-# NOINLINE fontCache #-} -- needed for unsafePerformIO
fontCache :: HT.HashTable (FilePath, Int) Font
fontCache = unsafePerformIO $ HT.new (==) hash
  where hash (str, int) = HT.hashString str `xor` HT.hashInt int

loadFont :: FilePath -> Int -> DrawOn z Font
loadFont name size = DrawOn $ do
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

loadSprite :: FilePath -> DrawOn z Sprite
loadSprite name = DrawOn $ do
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
  let pixelFormat = SDL.surfaceGetPixelFormat surface
  bmask <- SDLx.pixelFormatGetBmask pixelFormat
  let bgr = bmask == ntohl 0xff000000
  numColors <- SDL.pixelFormatGetBytesPerPixel pixelFormat
  let (format, format') = case numColors of
                            4 -> (if bgr then GL.BGRA else GL.RGBA, GL.RGBA')
                            3 -> (if bgr then GL.BGR  else GL.RGB,  GL.RGB')
                            _ -> error ("numColors = " ++ show numColors)
  let width = SDL.surfaceGetWidth surface
      height = SDL.surfaceGetHeight surface
  makeSprite width height $ do
    withForeignPtr surface $ const $ do
      pixelsPtr <- SDL.surfaceGetPixels surface
      GL.texImage2D Nothing GL.NoProxy 0 format'
          (GL.TextureSize2D (fromIntegral width) (fromIntegral height))
          0 (GL.PixelData format GL.UnsignedByte pixelsPtr)

-- | Convert a big-endian word to native endianness.
foreign import ccall unsafe "netinet/in.h" ntohl :: Word32 -> Word32

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

-------------------------------------------------------------------------------
