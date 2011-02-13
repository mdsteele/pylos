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

module Pylos.Event
  (Event(..), Key(..), KeyMod(..), fromSDLKey)
where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Extras as SDLx

import Pylos.Data.Point (IPoint)

-------------------------------------------------------------------------------

data Event = EvTick
           | EvQuit
           | EvKeyDown !Key [KeyMod] Char   -- ^ key, modifiers, character
           | EvKeyUp !Key                   -- ^ key
           | EvMouseMotion !IPoint !IPoint  -- ^ location, delta
           | EvMouseDown !IPoint            -- ^ location
           | EvMouseUp !IPoint              -- ^ location
  deriving (Eq, Show)

data KeyMod = KeyModCmd
            | KeyModShift
  deriving (Eq, Show)

data Key = KeyUnknown
         | KeyTab | KeyReturn | KeyEscape | KeySpace
         | Key0 | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9
         | KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ
         | KeyK | KeyL | KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT
         | KeyU | KeyV | KeyW | KeyX | KeyY | KeyZ
         | KeyUpArrow | KeyDownArrow | KeyRightArrow | KeyLeftArrow
  deriving (Eq, Show)

fromSDLKey :: SDL.SDLKey -> Key
fromSDLKey SDL.SDLK_TAB = KeyTab
fromSDLKey SDL.SDLK_RETURN = KeyReturn
fromSDLKey SDL.SDLK_ESCAPE = KeyEscape
fromSDLKey SDL.SDLK_SPACE = KeySpace
fromSDLKey SDL.SDLK_0 = Key0
fromSDLKey SDL.SDLK_1 = Key1
fromSDLKey SDL.SDLK_2 = Key2
fromSDLKey SDL.SDLK_3 = Key3
fromSDLKey SDL.SDLK_4 = Key4
fromSDLKey SDL.SDLK_5 = Key5
fromSDLKey SDL.SDLK_6 = Key6
fromSDLKey SDL.SDLK_7 = Key7
fromSDLKey SDL.SDLK_8 = Key8
fromSDLKey SDL.SDLK_9 = Key9
fromSDLKey SDL.SDLK_a = KeyA
fromSDLKey SDL.SDLK_b = KeyB
fromSDLKey SDL.SDLK_c = KeyC
fromSDLKey SDL.SDLK_d = KeyD
fromSDLKey SDL.SDLK_e = KeyE
fromSDLKey SDL.SDLK_f = KeyF
fromSDLKey SDL.SDLK_g = KeyG
fromSDLKey SDL.SDLK_h = KeyH
fromSDLKey SDL.SDLK_i = KeyI
fromSDLKey SDL.SDLK_j = KeyJ
fromSDLKey SDL.SDLK_k = KeyK
fromSDLKey SDL.SDLK_l = KeyL
fromSDLKey SDL.SDLK_m = KeyM
fromSDLKey SDL.SDLK_n = KeyN
fromSDLKey SDL.SDLK_o = KeyO
fromSDLKey SDL.SDLK_p = KeyP
fromSDLKey SDL.SDLK_q = KeyQ
fromSDLKey SDL.SDLK_r = KeyR
fromSDLKey SDL.SDLK_s = KeyS
fromSDLKey SDL.SDLK_t = KeyT
fromSDLKey SDL.SDLK_u = KeyU
fromSDLKey SDL.SDLK_v = KeyV
fromSDLKey SDL.SDLK_w = KeyW
fromSDLKey SDL.SDLK_x = KeyX
fromSDLKey SDL.SDLK_y = KeyY
fromSDLKey SDL.SDLK_z = KeyZ
fromSDLKey SDL.SDLK_UP = KeyUpArrow
fromSDLKey SDL.SDLK_DOWN = KeyDownArrow
fromSDLKey SDL.SDLK_RIGHT = KeyRightArrow
fromSDLKey SDL.SDLK_LEFT = KeyLeftArrow
fromSDLKey _ = KeyUnknown

-------------------------------------------------------------------------------
