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

module Pylos.Views.Title
  (TitleAction(..), newTitleView)
where

import Pylos.Data.Point (IPoint, Rect(Rect), pZero)
import Pylos.Draw (DrawOn, blitTopleft, loadSprite)
import Pylos.Event (Key(KeyN, KeyQ))
import Pylos.Views (View, compoundView, inertView, subView_, viewMap)
import Pylos.Views.Widgets (newTextButton)

-------------------------------------------------------------------------------

data TitleAction = NewGame | QuitGame

newTitleView :: DrawOn z (View () TitleAction)
newTitleView = do
  background <- newBackgroundView
  newGameButton <- fmap (subView_ (Rect 480 350 100 30) .
                         viewMap (const ("New Game", True)) id) $
                   newTextButton (Just KeyN) NewGame
  quitButton <- fmap (subView_ (Rect 480 400 100 30) .
                         viewMap (const ("Quit", True)) id) $
                   newTextButton (Just KeyQ) QuitGame
  return $ compoundView [background, newGameButton, quitButton]

-------------------------------------------------------------------------------

newBackgroundView :: DrawOn z (View a b)
newBackgroundView = do
  background <- loadSprite "title.png"
  return $ inertView $ const $ blitTopleft background (pZero :: IPoint)

-------------------------------------------------------------------------------
