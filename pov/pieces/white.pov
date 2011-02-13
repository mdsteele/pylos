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
#include "woods.inc"

///////////////////////////////////////////////////////////////////////////////

sphere {
  0, 1
  texture {
    T_Wood7
    rotate <10, 75, 60>
    translate 0.5*y
    translate x
  }
}

///////////////////////////////////////////////////////////////////////////////

camera {
  right (x / 4.7)
  up (y / 4.7)
  location (-10 * z)
  look_at <0, 0, 0>
}

light_source {
  <8, 8, -60>
  color rgb 2
}

///////////////////////////////////////////////////////////////////////////////
