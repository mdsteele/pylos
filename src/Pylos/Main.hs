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

module Pylos.Main (sdlMain) where

import Control.Exception (bracket_)
import Control.Monad ((>=>))
import Data.List (nub)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Framerate as SDLf
import qualified Graphics.UI.SDL.Mixer as SDLm
import qualified Graphics.UI.SDL.TTF as SDLt

import Pylos.Constants (framesPerSecond)
import Pylos.Data.Point (Point(Point))
import Pylos.Draw (initializeScreen)
import Pylos.Event
import Pylos.Modes (Mode, NextMode(..), newBootUpMode)

-------------------------------------------------------------------------------

data EngineState = EngineState { engineFps :: SDLf.FPSManager,
                                 engineClockOn :: Bool,
                                 engineMode :: Mode }

newEngineState :: IO EngineState
newEngineState = do fps <- SDLf.new
                    SDLf.init fps
                    SDLf.set fps framesPerSecond
                    mode <- newBootUpMode
                    return $ EngineState fps True mode

convertKeyMods :: [SDL.Modifier] -> Maybe [KeyMod]
convertKeyMods = fmap nub . sequence . map convert where
  convert :: SDL.Modifier -> Maybe KeyMod
  convert SDL.KeyModLeftShift  = Just KeyModShift
  convert SDL.KeyModRightShift = Just KeyModShift
  convert SDL.KeyModShift      = Just KeyModShift
  convert SDL.KeyModLeftMeta  = Just KeyModCmd
  convert SDL.KeyModRightMeta = Just KeyModCmd
  convert SDL.KeyModMeta      = Just KeyModCmd
  convert _ = Nothing

-------------------------------------------------------------------------------

eventLoop :: EngineState -> IO ()
eventLoop state = SDL.pollEvent >>= handleEvent where

  handleEvent :: SDL.Event -> IO ()
  handleEvent SDL.NoEvent =
    do let mode = engineMode state
       nxt <- if engineClockOn state then mode EvTick else return SameMode
       SDLf.delay (engineFps state)
       nextMode nxt
  handleEvent (SDL.GotFocus focus) =
    if SDL.InputFocus `notElem` focus then ignore
    else eventLoop $ state {engineClockOn = True}
  handleEvent (SDL.LostFocus focus) =
    if SDL.InputFocus `notElem` focus then ignore
    else eventLoop $ state {engineClockOn = False}
  handleEvent (SDL.KeyDown (SDL.Keysym k ms char)) =
    let handle mods = if mods == [KeyModCmd] && k == SDL.SDLK_q
                      then passToMode EvQuit
                      else passToMode $ EvKeyDown (fromSDLKey k) mods char
    in maybe ignore handle (convertKeyMods ms)
  handleEvent (SDL.KeyUp (SDL.Keysym k _ _)) =
    passToMode . EvKeyUp $ fromSDLKey k
  handleEvent (SDL.MouseMotion x y dx dy) =
    passToMode $ EvMouseMotion (Point (fromIntegral x) (fromIntegral y))
                               (Point (fromIntegral dx) (fromIntegral dy))
  handleEvent (SDL.MouseButtonDown x y button) =
    if button /= SDL.ButtonLeft then ignore
    else passToMode . EvMouseDown $ Point (fromIntegral x) (fromIntegral y)
  handleEvent (SDL.MouseButtonUp x y button) =
    if button /= SDL.ButtonLeft then ignore
    else passToMode . EvMouseUp $ Point (fromIntegral x) (fromIntegral y)
  handleEvent SDL.Quit = passToMode EvQuit
  handleEvent _ = ignore

  ignore :: IO ()
  ignore = eventLoop state

  passToMode :: Event -> IO ()
  passToMode = engineMode state >=> nextMode

  nextMode :: NextMode -> IO ()
  nextMode DoQuit = return ()
  nextMode SameMode = eventLoop state
  nextMode (ChangeMode mode) = eventLoop $ state { engineMode = mode }

-------------------------------------------------------------------------------

sdlMain :: IO ()
sdlMain = withSdlInit $ do
  initializeScreen False
  let windowTitle = "Pylos"
  SDL.setCaption windowTitle windowTitle
  SDL.enableUnicode True
  newEngineState >>= eventLoop

withSdlInit :: IO a -> IO a
withSdlInit = SDL.withInit [SDL.InitEverything] . withTtfInit . withMixerInit
  where
    withTtfInit m = SDLt.init >>= \ok -> if ok then m
                                         else fail "SDL.TTF.init failed"
    withMixerInit = bracket_ (SDLm.openAudio SDLm.defaultFrequency
                                             SDLm.AudioS16Sys 2 1024)
                             SDLm.closeAudio

-------------------------------------------------------------------------------
