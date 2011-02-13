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

module Pylos.Data.Color where

import Data.Word (Word8)

-------------------------------------------------------------------------------

data Color = Color { colorRed :: !Word8,
                     colorGreen :: !Word8,
                     colorBlue :: !Word8 }

whiteColor :: Color
whiteColor = Color 255 255 255

-------------------------------------------------------------------------------

data Tint = Tint { tintRed :: !Word8,
                   tintGreen :: !Word8,
                   tintBlue :: !Word8,
                   tintAlpha :: !Word8 }

fromTint :: Tint -> Color
fromTint (Tint r g b _) = Color r g b

-------------------------------------------------------------------------------
