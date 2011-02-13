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

module Pylos.Modes
  (Mode, NextMode(..), newBootUpMode)
where

import Control.Monad (when)
import Data.IORef

import Pylos.Constants (screenRect)
import Pylos.Draw (runDraw)
import Pylos.Event
import qualified Pylos.Modes.Gameplay as GameplayMode
import Pylos.State (GameState, newGameState)
import Pylos.Views (View, paintScreen, viewHandler)
import Pylos.Views.Board (newGameplayView)
import Pylos.Views.NewGame (NewGameAction(..), newNewGameView)
import Pylos.Views.Title (TitleAction(NewGame, QuitGame), newTitleView)

-------------------------------------------------------------------------------

type Mode = Event -> IO NextMode

data NextMode = DoQuit | SameMode | ChangeMode Mode

-------------------------------------------------------------------------------

newBootUpMode :: IO Mode
newBootUpMode = return mode where
  mode EvQuit = return DoQuit
  mode EvTick = fmap ChangeMode newTitleMode
  mode _ = return SameMode

-------------------------------------------------------------------------------

newTitleMode :: IO Mode
newTitleMode = do
  view <- runDraw newTitleView
  let mode EvQuit = return DoQuit
      mode event = do
        mbAction <- runDraw $ viewHandler view () screenRect event
        when (event == EvTick) $ paintScreen view ()
        case mbAction of
          Nothing -> return SameMode
          Just NewGame -> fmap ChangeMode $ newNewGameMode mode view ()
--             state <- newGameState
--             mode' <- newGameplayMode state
--             return (ChangeMode mode')
          Just QuitGame -> return DoQuit
  return mode

-------------------------------------------------------------------------------

newNewGameMode :: Mode -> View a b -> a -> IO Mode
newNewGameMode prevMode bgView bgInput = do
  view <- runDraw $ newNewGameView bgView bgInput
  let mode EvQuit = return DoQuit
      mode event = do
        mbAction <- runDraw $ viewHandler view () screenRect event
        when (event == EvTick) $ do
          _ <- runDraw $ viewHandler bgView bgInput screenRect event
          paintScreen view ()
        case mbAction of
          Nothing -> return SameMode
          Just CancelNewGame -> return $ ChangeMode prevMode
          Just (StartNewGame spec) -> do
            state <- newGameState spec
            mode' <- newGameplayMode state
            return (ChangeMode mode')
  return mode

-------------------------------------------------------------------------------

newGameplayMode :: GameState -> IO Mode
newGameplayMode initState = do
  view <- runDraw newGameplayView
  stateRef <- newIORef initState
  let mode EvQuit = return DoQuit
      mode event = do
        when (event == EvTick) $ do
          state <- readIORef stateRef
          state' <- GameplayMode.performTick state
          writeIORef stateRef state'
        state <- readIORef stateRef
        mbAction <- runDraw $ viewHandler view state screenRect event
        when (event == EvTick) $ paintScreen view state
        case mbAction of
          Nothing -> return SameMode
          Just action ->
            case GameplayMode.handleAction state action of
              Nothing -> fmap ChangeMode newTitleMode
              Just state' -> writeIORef stateRef state' >> return SameMode
  return mode

-------------------------------------------------------------------------------
