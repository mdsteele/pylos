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

module Pylos.Data.Point where

import Control.Monad (liftM2)
import Test.QuickCheck (Arbitrary(arbitrary, coarbitrary))

-------------------------------------------------------------------------------

class (Real a) => Axis a where
  half :: a -> a
  toIntegral :: (Integral b) => a -> b
  toFloating :: (RealFloat b) => a -> b

instance Axis Int where
  half = (`div` 2)
  toIntegral = fromIntegral
  toFloating = fromIntegral

instance Axis Double where
  half = (/ 2)
  toIntegral = round
  toFloating = realToFrac

-------------------------------------------------------------------------------

type IPoint = Point Int
type DPoint = Point Double

data Point a = Point { pointX :: !a, pointY :: !a }
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Point a) where
  arbitrary = liftM2 Point arbitrary arbitrary
  coarbitrary (Point x y) = coarbitrary x . coarbitrary y

instance Functor Point where
  fmap f (Point x y) = Point (f x) (f y)

pZero :: (Num a) => Point a
pZero = Point 0 0

infixl 6 `pAdd`, `pSub`

pAdd :: (Num a) => Point a -> Point a -> Point a
pAdd (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

pSub :: (Num a) => Point a -> Point a -> Point a
pSub (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

pNeg :: (Num a) => Point a -> Point a
pNeg (Point x y) = Point (negate x) (negate y)

-------------------------------------------------------------------------------

type IRect = Rect Int
type DRect = Rect Double

data Rect a = Rect { rectX :: !a, rectY :: !a, rectW :: !a, rectH :: !a }
  deriving (Eq, Show)

instance Functor Rect where
  fmap f (Rect x y w h) = Rect (f x) (f y) (f w) (f h)

-- | Return 'True' if the rectangle contains the given point, 'False'
--   otherwise.
rectContains :: (Real a) => Rect a -> Point a -> Bool
rectContains (Rect x y w h) (Point x' y') =
  x' >= x && y' >= y && x' < x + w && y' < y + h

-- | Find the intersection of two rectangles.
rectIntersection :: (Real a) => Rect a -> Rect a -> Rect a
rectIntersection (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  let x = max x1 x2
      y = max y1 y2
      w = max 0 (min (x1 + w1) (x2 + w2) - x)
      h = max 0 (min (y1 + h1) (y2 + h2) - y)
  in Rect x y w h

rectPlus :: (Num a) => Rect a -> Point a -> Rect a
rectPlus (Rect x1 y1 w h) (Point x2 y2) = Rect (x1 + x2) (y1 + y2) w h

rectMinus :: (Num a) => Rect a -> Point a -> Rect a
rectMinus (Rect x1 y1 w h) (Point x2 y2) = Rect (x1 - x2) (y1 - y2) w h

adjustRect :: (Real a) => a -> a -> a -> a -> Rect a -> Rect a
adjustRect x1 y1 x2 y2 (Rect x y w h) =
  Rect (x + x1) (y + y1) (max 0 $ w - x2 - x1) (max 0 $ h - y2 - y1)

-- | Return the size of the rectangle as a @(width, height)@ pair.
rectSize :: Rect a -> (a, a)
rectSize (Rect _ _ w h) = (w, h)

-- | Return the point at the top-left corner of the given rectangle.
rectTopleft :: Rect a -> Point a
rectTopleft (Rect x y _ _) = Point x y

-- | Return the point at the top-left corner of the given rectangle.
rectCenter :: (Axis a) => Rect a -> Point a
rectCenter (Rect x y w h) = Point (x + half w) (y + half h)

-------------------------------------------------------------------------------

data LocSpec a = LocTopleft !(Point a)
               | LocTopright !(Point a)
               | LocMidleft !(Point a)
               | LocCenter !(Point a)
               | LocMidright !(Point a)
               | LocBottomleft !(Point a)
               | LocBottomright !(Point a)

locTopleft :: (Axis a) => LocSpec a -> (a, a) -> Point a
locTopleft loc (w, h) =
  case loc of
    LocTopleft p -> p
    LocTopright (Point x y) -> Point (x - w) y
    LocMidleft (Point x y) -> Point x (y - half h)
    LocCenter (Point x y) -> Point (x - half w) (y - half h)
    LocMidright (Point x y) -> Point (x - w) (y - half h)
    LocBottomleft (Point x y) -> Point x (y - h)
    LocBottomright (Point x y) -> Point (x - w) (y - h)

locRect :: (Axis a) => LocSpec a -> (a, a) -> Rect a
locRect loc (w, h) = Rect x y w h where
  Point x y = locTopleft loc (w, h)

-------------------------------------------------------------------------------
