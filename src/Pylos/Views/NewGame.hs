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

module Pylos.Views.NewGame
  (NewGameAction(..), newNewGameView)
where

import Control.Monad (when)

import Pylos.Data.Point
import Pylos.Draw
import Pylos.Event
import Pylos.Game (Ruleset(..))
import Pylos.State (NewGameSpec(..), Player(..))
import Pylos.Views
import Pylos.Views.Widgets (newLabel, newRadioButton, newTextButton)

-------------------------------------------------------------------------------

data NewGameAction = CancelNewGame | StartNewGame NewGameSpec

-------------------------------------------------------------------------------

newNewGameView :: View a b -> a -> DrawOn z (View () NewGameAction)
newNewGameView bgView bgInput = do
  dialog <- fmap (subView_ $ Rect 140 110 360 260) newNewGameDialog
  let paint input = do
        viewPaint bgView bgInput
        viewPaint dialog input
      handler input rect event = do
        when (event == EvTick) $ do
          _ <- viewHandler bgView bgInput rect event
          return ()
        viewHandler dialog input rect event
  return $ View paint handler

-------------------------------------------------------------------------------

newNewGameDialog :: DrawOn z (View () NewGameAction)
newNewGameDialog = do
  rulesetRef <- newDrawRef NormalRules
  boardSizeRef <- newDrawRef 4
  whiteRef <- newDrawRef HumanPlayer
  blackRef <- newDrawRef CpuPlayer
  let newStartAction = do
        ruleset <- readDrawRef rulesetRef
        boardSize <- readDrawRef boardSizeRef
        whitePlayer <- readDrawRef whiteRef
        blackPlayer <- readDrawRef blackRef
        return $ StartNewGame $ NewGameSpec { ngRuleset = ruleset,
                                              ngBoardSize = boardSize,
                                              ngWhitePlayer = whitePlayer,
                                              ngBlackPlayer = blackPlayer }
  fmap compoundView $ sequence $ [
    newDialogBackgroundView,
    -- Ruleset controls:
    (fmap (viewMap (const "Ruleset:") id) $
     newLabel $ LocMidleft $ (Point 20 34 :: IPoint)),
    (fmap (subView_ (Rect 90 24 75 20) . viewMap (const "Simple") id) $
     newRadioButton rulesetRef SimpleRules),
    (fmap (subView_ (Rect 175 24 75 20) . viewMap (const "Normal") id) $
     newRadioButton rulesetRef NormalRules),
    (fmap (subView_ (Rect 260 24 75 20) . viewMap (const "Expert") id) $
     newRadioButton rulesetRef ExpertRules),
    -- Board size controls:
    (fmap (viewMap (const "Board size:") id) $
     newLabel $ LocMidleft $ (Point 20 79 :: IPoint)),
    (fmap (subView_ (Rect 110 69 50 20) . viewMap (const "3x3") id) $
     newRadioButton boardSizeRef 3),
    (fmap (subView_ (Rect 170 69 50 20) . viewMap (const "4x4") id) $
     newRadioButton boardSizeRef 4),
    (fmap (subView_ (Rect 230 69 50 20) . viewMap (const "5x5") id) $
     newRadioButton boardSizeRef 5),
    (fmap (subView_ (Rect 290 69 50 20) . viewMap (const "6x6") id) $
     newRadioButton boardSizeRef 6),
    -- White player controls:
    (fmap (viewMap (const "White player:") id) $
     newLabel $ LocMidleft $ (Point 20 124 :: IPoint)),
    (fmap (subView_ (Rect 132 114 80 20) . viewMap (const "Human") id) $
     newRadioButton whiteRef HumanPlayer),
    (fmap (subView_ (Rect 222 114 80 20) . viewMap (const "CPU") id) $
     newRadioButton whiteRef CpuPlayer),
    -- Black player controls:
    (fmap (viewMap (const "Black player:") id) $
     newLabel $ LocMidleft $ (Point 20 169 :: IPoint)),
    (fmap (subView_ (Rect 132 159 80 20) . viewMap (const "Human") id) $
     newRadioButton blackRef HumanPlayer),
    (fmap (subView_ (Rect 222 159 80 20) . viewMap (const "CPU") id) $
     newRadioButton blackRef CpuPlayer),
    -- Bottom buttons:
    (fmap (subView_ (Rect 244 220 100 24) .
           viewMap (const ("Start", True)) id .
           inject newStartAction) $
     newTextButton (Just KeyReturn) ()),
    (fmap (subView_ (Rect 16 220 100 24) .
           viewMap (const ("Cancel", True)) id) $
     newTextButton (Just KeyEscape) CancelNewGame)]

-------------------------------------------------------------------------------

newDialogBackgroundView :: DrawOn z (View a b)
newDialogBackgroundView = do
  image <- loadSprite "dialog.png"
  topleft <- subSprite image $ Rect 0 0 8 8
  topright <- subSprite image $ Rect 264 0 8 8
  bottomleft <- subSprite image $ Rect 0 264 8 8
  bottomright <- subSprite image $ Rect 264 264 8 8
  top <- subSprite image $ Rect 8 0 256 8
  bottom <- subSprite image $ Rect 8 264 256 8
  left <- subSprite image $ Rect 0 8 8 256
  right <- subSprite image $ Rect 264 8 8 256
  center <- subSprite image $ Rect 8 8 256 256
  let paint _ = do
        rect <- canvasRect
        let (width, height) = rectSize rect
        -- Center:
        blitRepeat center (Point 50 0) $ adjustRect 8 8 8 8 rect
        -- Sides:
        blitRepeat top pZero $ Rect 8 0 (width - 16) 8
        blitRepeat bottom pZero $ Rect 8 (height - 8) (width - 16) 8
        blitRepeat left pZero $ Rect 0 8 8 (height - 16)
        blitRepeat right pZero $ Rect (width - 8) 8 8 (height - 16)
        -- Corners:
        blitLoc topleft $ LocTopleft $ Point 0 (0 :: Int)
        blitLoc topright $ LocTopright $ Point width 0
        blitLoc bottomleft $ LocBottomleft $ Point 0 height
        blitLoc bottomright $ LocBottomright $ Point width height
  return $ inertView paint

-------------------------------------------------------------------------------

inject :: (DrawOn () c) -> View a b -> View a c
inject action (View paint handler) = View paint handler' where
  handler' input rect event = do
    mbValue <- handler input rect event
    case mbValue of
      Nothing -> return Nothing
      Just _ -> fmap Just action

-------------------------------------------------------------------------------
