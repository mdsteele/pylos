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

module Pylos.Constants where

import Pylos.Data.Point (IRect, Rect(Rect))

-------------------------------------------------------------------------------
-- Display constants:

framesPerSecond :: Int
framesPerSecond = 40

secondsPerFrame :: Double
secondsPerFrame = recip (fromIntegral framesPerSecond)

screenWidth, screenHeight :: Int
screenWidth = 640
screenHeight = 480

screenRect :: IRect
screenRect = Rect 0 0 screenWidth screenHeight

-------------------------------------------------------------------------------
