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

#include <stdio.h>
#include "HsFFI.h"

#ifdef __APPLE__
#include <objc/objc.h>
#include <objc/objc-runtime.h>
#endif

#include <SDL/SDL.h>

#ifdef __GLASGOW_HASKELL__
#include "HSMain_stub.h"
extern void __stginit_HSMain (void);
#endif

int SDL_main(int argc, char *argv[]) {
#ifdef __APPLE__
  void* pool = objc_msgSend(objc_lookUpClass("NSAutoreleasePool"),
                            sel_getUid("alloc"));
  objc_msgSend(pool, sel_getUid("init"));
#endif

  hs_init(&argc, &argv);

#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_HSMain);
#endif

  hs_main();

  hs_exit();

  return 0;
}
