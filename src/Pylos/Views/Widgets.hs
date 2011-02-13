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

newLabel :: (Axis a) => LocSpec a -> DrawOn z (View String b)
newLabel loc = do
  font <- loadFont "caligula.ttf" 18
  let paint text = drawText font whiteColor loc text
  return $ inertView paint

-------------------------------------------------------------------------------

newTextButton :: Maybe Key -> a -> DrawOn z (View (String, Bool) a)
newTextButton mbKey value = do
  state <- newDrawRef (False, False, False)
  font <- loadFont "caligula.ttf" 18
  let

    paint (text, enabled) = do
      (keyPressed, mousePressed, mouseHover) <- readDrawRef state
      rect <- canvasRect
      let pressed = enabled && (keyPressed || mousePressed && mouseHover)
      let tint = if not enabled then Tint 128 128 128 128
                 else if pressed then Tint 64 64 255 255
                      else if mouseHover then Tint 64 255 255 255
                           else Tint 255 255 255 255
      drawBevelRect tint 5 rect
      let center = rectCenter rect `pAdd` if pressed then Point 1 1 else pZero
      drawText font (fromTint tint) (LocCenter center) text

    handler (_, enabled) rect event = do
      (kp, mp, mh) <- readDrawRef state
      case event of
        EvKeyDown key kmod _ -> do
          when (null kmod && mbKey == Just key) $ do
            writeDrawRef state (True, mp, mh)
          return Nothing
        EvKeyUp key ->
          if not (kp && mbKey == Just key) then return Nothing
          else writeDrawRef state (False, mp, mh) >> return (Just value)
        EvMouseMotion pt _ ->
          writeDrawRef state (kp, mp, rectContains rect pt) >> return Nothing
        EvMouseDown pt -> do
          when (rectContains rect pt) $ writeDrawRef state (kp, True, mh)
          return Nothing
        EvMouseUp _ -> if not mp then return Nothing else do
          writeDrawRef state (kp, False, mh)
          return $ if mh && not kp && enabled then Just value else Nothing
        _ -> return Nothing

  return (View paint handler)

-------------------------------------------------------------------------------

newRadioButton :: (Eq a) => DrawRef a -> a -> DrawOn z (View String b)
newRadioButton valueRef value = do
  hoverRef <- newDrawRef False
  font <- loadFont "caligula.ttf" 18
  let

    paint text = do
      mouseHover <- readDrawRef hoverRef
      current <- readDrawRef valueRef
      let pressed = current == value
      let tint = if pressed then Tint 255 64 64 255
                 else if mouseHover then Tint 255 255 64 255
                      else Tint 255 255 255 255
      rect <- canvasRect
      drawBevelRect tint 5 rect
      let center = rectCenter rect `pAdd` if pressed then Point 1 1 else pZero
      drawText font (fromTint tint) (LocCenter center) text

    handler _ rect event = do
      case event of
        EvMouseMotion pt _ ->
          writeDrawRef hoverRef (rectContains rect pt)
        EvMouseDown pt -> do
          when (rectContains rect pt) $ writeDrawRef valueRef value
        _ -> return ()
      return Nothing

  return (View paint handler)

-------------------------------------------------------------------------------

drawBevelRect :: Tint -> Int -> IRect -> Draw ()
drawBevelRect tint b rect = withSubCanvas rect $ do
  (w, h) <- canvasSize
  drawPolygon tint [Point b 0, Point (w - b - 1) 0,
                    Point (w - 1) b, Point (w - 1) (h - b - 1),
                    Point (w - b - 1) (h - 1), Point b (h - 1),
                    Point 0 (h - b - 1), Point 0 b]

-------------------------------------------------------------------------------
