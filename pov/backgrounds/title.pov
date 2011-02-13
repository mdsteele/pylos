/*=============================================================================
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
=============================================================================*/

#include "colors.inc"
#include "stones1.inc"
#include "woods.inc"

///////////////////////////////////////////////////////////////////////////////

plane {
  y, 0
  texture { T_Grnt15 scale 10 }
}

text {
  ttf "caligula.ttf" "Pylos" 1000, 0
  texture { T_Stone13 scale 0.5 }
  scale 2
  scale <1.1, 1, 1>
  translate <-5.5, 6.2, -3>
  no_shadow
}

#macro Piece(level, row, col, isWhite)
  #local rad = 1;
  #local diam = 2 * rad;
  #local center = (y*sqrt(2)*rad*(4 - level) + y*rad +
    (x + z)*(-3*rad) +
    (x + z)*rad*(4 - level) + x*diam*row + z*diam*col);
  sphere {
    center, rad
    texture {
      #if (isWhite)
        T_Wood7
      #else
        T_Wood12
      #end
      rotate 10*y
    }
  }
#end

#macro Pyramid(arr)
  union {
    #local level = 4;
    #local row = 0;
    #local col = 0;
    #local index = 0;
    #while (index < 30)
      Piece(level, row, col, arr[index])
      #local col = col + 1;
      #if (col >= level)
        #local col = 0;
        #local row = row + 1;
        #if (row >= level)
          #local row = 0;
          #local level = level - 1;
        #end
      #end
      #local index = index + 1;
    #end
  }
#end

#declare pieces = array[30] {
  true,  false,  false, true,
  false, true,  true,  false,
  false, false, false, true,
  true,  true,  false, true,

  true,  false, false,
  true,  false, true,
  false, true,  false,

  true,  false,
  true,  true,

  false
};
object {
  Pyramid(pieces)
  scale 0.95
  rotate -225*y
  translate -1*x
}

///////////////////////////////////////////////////////////////////////////////

camera {
  right x*4/3
  up y
  location <-4, 6, -12>
  look_at <0, 3, 0>
}

light_source {
  <3, 30, -20>
  color rgb 1
}

light_source {
  <-3, 30, -50>
  color rgb 1.5
}

///////////////////////////////////////////////////////////////////////////////
