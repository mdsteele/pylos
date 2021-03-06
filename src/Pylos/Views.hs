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

module Pylos.Views
  (-- * The @View@ type
   View(..), inertView,
   -- * Painting
   paintScreen, handleEvent,
   -- * View combinators
   compoundView, subView, subView_, viewMap,
   -- * Utility functions
   translateEvent)
where

import Data.Maybe (catMaybes, listToMaybe)

import Pylos.Constants (screenRect)
import Pylos.Data.Point (IRect, pSub, rectTopleft)
import Pylos.Draw
import Pylos.Event
import Pylos.Utility (flip3)

-------------------------------------------------------------------------------
-- The View type:

data View a b = View
  { viewPaint :: a -> Paint (),
    viewHandler :: a -> Event -> Handler (Maybe b) }

inertView :: (a -> Paint ()) -> View a b
inertView paintFn = View paintFn $ const $ const $ return Nothing

-------------------------------------------------------------------------------
-- Painting:

paintScreen :: View a b -> a -> IO ()
paintScreen view input = drawToScreen (viewPaint view input)

handleEvent :: View a b -> a -> Event -> IO (Maybe b)
handleEvent view input event =
  runHandlerIO (viewHandler view input event) screenRect

-------------------------------------------------------------------------------
-- View combinators:

compoundView :: [View a b] -> View a b
compoundView views = View paintFn handlerFn where
  paintFn input = mapM_ (flip viewPaint input) views
  handlerFn input event = do
    results <- mapM (flip3 viewHandler input event) views
    return $ listToMaybe $ reverse $ catMaybes results

subView :: (a -> (Int, Int) -> IRect) -> View a b -> View a b
subView rectFn view = View paintFn handlerFn where
  paintFn input = do size <- canvasSize
                     withSubCanvas (rectFn input size) (viewPaint view input)
  handlerFn input event = do
    size <- canvasSize
    let subrect = rectFn input size
    withSubCanvas subrect $ do
      viewHandler view input $ translateEvent subrect event

subView_ :: IRect -> View a b -> View a b
subView_ = subView . const . const

viewMap :: (a -> c) -> (d -> b) -> View c d -> View a b
viewMap f1 f2 (View paint handler) = View paint' handler' where
  paint' = paint . f1
  handler' input event = do
    mbValue <- handler (f1 input) event
    return $ fmap f2 mbValue

-------------------------------------------------------------------------------
-- Utility functions:

translateEvent :: IRect -> Event -> Event
translateEvent rect event =
  case event of
    EvMouseMotion pt rel -> EvMouseMotion (translate pt) rel
    EvMouseUp pt -> EvMouseUp (translate pt)
    EvMouseDown pt -> EvMouseDown (translate pt)
    _ -> event
  where translate = (flip pSub) (rectTopleft rect)

-------------------------------------------------------------------------------
