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

module Pylos.Views.Widgets
  (newLabel, newTextButton, newRadioButton, drawBevelRect)
where

import Control.Monad (when)

import Pylos.Data.Color (Tint(Tint), fromTint, whiteColor)
import Pylos.Data.Point
import Pylos.Draw
import Pylos.Event
import Pylos.Views

-------------------------------------------------------------------------------

newLabel :: (Axis a, MonadDraw m) => LocSpec a -> m (View String b)
newLabel loc = do
  font <- loadFont "caligula.ttf" 18
  let paint text = drawText font whiteColor loc text
  return $ inertView paint

-------------------------------------------------------------------------------

newTextButton :: (MonadDraw m) => Maybe Key -> a -> m (View (String, Bool) a)
newTextButton mbKey value = do
  state <- newDrawRef (False, False)
  font <- loadFont "caligula.ttf" 18
  let

    paint (text, enabled) = do
      (keyPressed, mousePressed) <- readDrawRef state
      rect <- canvasRect
      mouseHover <- fmap (rectContains rect) getRelativeMousePos
      let pressed = enabled && (keyPressed || mousePressed && mouseHover)
      let tint = if not enabled then Tint 128 128 128 128
                 else if pressed then Tint 64 64 255 255
                      else if mouseHover then Tint 64 255 255 255
                           else Tint 255 255 255 255
      drawBevelRect tint 5 rect
      let center = rectCenter rect `pAdd` if pressed then Point 1 1 else pZero
      drawText font (fromTint tint) (LocCenter center) text

    handler (_, enabled) event = do
      (kp, mp) <- readDrawRef state
      rect <- canvasRect
      case event of
        EvKeyDown key kmod _ -> do
          when (null kmod && mbKey == Just key) $ do
            writeDrawRef state (True, mp)
          return Nothing
        EvKeyUp key ->
          if not (kp && mbKey == Just key) then return Nothing
          else writeDrawRef state (False, mp) >> return (Just value)
        EvMouseDown pt -> do
          when (rectContains rect pt) $ writeDrawRef state (kp, True)
          return Nothing
        EvMouseUp pt -> if not mp then return Nothing else do
          writeDrawRef state (kp, False)
          return $ if rectContains rect pt && not kp && enabled
                   then Just value else Nothing
        _ -> return Nothing

  return (View paint handler)

-------------------------------------------------------------------------------

newRadioButton :: (Eq a, MonadDraw m) => DrawRef a -> a -> m (View String b)
newRadioButton valueRef value = do
  font <- loadFont "caligula.ttf" 18
  let

    paint text = do
      rect <- canvasRect
      mouseHover <- fmap (rectContains rect) getRelativeMousePos
      current <- readDrawRef valueRef
      let pressed = current == value
      let tint = if pressed then Tint 255 64 64 255
                 else if mouseHover then Tint 255 255 64 255
                      else Tint 255 255 255 255
      drawBevelRect tint 5 rect
      let center = rectCenter rect `pAdd` if pressed then Point 1 1 else pZero
      drawText font (fromTint tint) (LocCenter center) text

    handler _ event = do
      case event of
        EvMouseDown pt -> do
          rect <- canvasRect
          when (rectContains rect pt) $ writeDrawRef valueRef value
        _ -> return ()
      return Nothing

  return (View paint handler)

-------------------------------------------------------------------------------

drawBevelRect :: Tint -> Int -> IRect -> Paint ()
drawBevelRect tint b rect = withSubCanvas rect $ do
  (w, h) <- canvasSize
  drawPolygon tint [Point b 0, Point (w - b - 1) 0,
                    Point (w - 1) b, Point (w - 1) (h - b - 1),
                    Point (w - b - 1) (h - 1), Point b (h - 1),
                    Point 0 (h - b - 1), Point 0 b]

-------------------------------------------------------------------------------
