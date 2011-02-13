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

union {
  plane {
    -z, 0
    texture { T_Grnt15 scale 10 }
  }
  prism {
    linear_spline
    conic_sweep
    1, 0.9, 5,
    <1, 1>, <-1, 1>, <-1, -1>, <1, -1>, <1, 1>
    translate -y
    rotate 90*x
    scale 4.4
    translate y*0.5
    translate -z*0.1
    texture { T_Grnt15 scale 1 }
  }
}

///////////////////////////////////////////////////////////////////////////////

camera {
  right x*4/3
  up y
  location <0, 0, -12>
  look_at <0, 0, 0>
}

light_source {
  <-6, 10, -30>
  color rgb 1.3
}

///////////////////////////////////////////////////////////////////////////////
