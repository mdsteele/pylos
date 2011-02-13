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

module Pylos.Data.TotalMap
  (TotalMap, makeTotalMap, makeTotalMap_, tmGet, tmSet)
where

import Control.Applicative (Applicative(pure, (<*>)))
import Control.Arrow ((***))
import Control.Exception (assert)
import Data.Array ((!), (//), Array, listArray)
import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav

-------------------------------------------------------------------------------

-- | A 'TotalMap' is a key-value map whose key space is a finite set --
--   specifically, a type that is an instance of the 'Enum' and 'Bounded'
--   classes.  The abstraction guarantees that a value will always be present
--   in the 'TotalMap' for every element of the key space, and that lookup is
--   constant-time.
newtype TotalMap a b = TotalMap (Array Int b)
  deriving (Eq, Ord, Show)

instance Functor (TotalMap a) where
  fmap fn (TotalMap arr) = TotalMap (fmap fn arr)

instance (Bounded a, Enum a) => Applicative (TotalMap a) where
  pure = makeTotalMap_
  (TotalMap arr1) <*> (TotalMap arr2) =
    let apply ix = (arr1 ! ix) (arr2 ! ix)
        create rng = map apply $ uncurry enumFromTo rng
    in makeTotalMap' create

instance Fold.Foldable (TotalMap a) where
  foldr fn start (TotalMap arr) = Fold.foldr fn start arr

instance Trav.Traversable (TotalMap a) where
  traverse fn (TotalMap arr) = fmap TotalMap $ Trav.traverse fn arr

-- | Create a new 'TotalMap' with each item initialized by applying the
--   function to the corresponding key.  This function is strict in the return
--   values of the function passed (to ensure that 'tmGet' never evaluates to
--   bottom).
makeTotalMap :: (Bounded a, Enum a) => (a -> b) -> TotalMap a b
makeTotalMap fn = makeTotalMap' create
  where create rng = map (force . fn) $ uncurry enumFromTo $
                     (toEnum *** toEnum) rng
        force x = x `seq` x

-- | Create a new 'TotalMap' with all items set to the initial value.
--   Equivalent to @'makeTotalMap' . const@.  This function is strict in the
--   value passed (to ensure that 'tmGet' never evaluates to bottom).
makeTotalMap_ :: (Bounded a, Enum a) => b -> TotalMap a b
makeTotalMap_ value = value `seq` (makeTotalMap' $ const $ repeat value)

-- | Get an item from a 'TotalMap'.  Assuming that the key type is
--   well-behaved, this function will never fail or return bottom.
tmGet :: (Bounded a, Enum a) => a -> TotalMap a b -> b
tmGet key (TotalMap arr) = assert (inRange key) $ arr ! fromEnum key

-- | Set an item in a 'TotalMap'.  Assuming that the key type is well-behaved,
--   this function will never fail or return bottom.
tmSet :: (Bounded a, Enum a) => b -> a -> TotalMap a b -> TotalMap a b
tmSet value key (TotalMap arr) =
  assert (inRange key) $ TotalMap $ arr // [(fromEnum key, value)]

-------------------------------------------------------------------------------
-- Private utility functions:

inRange :: (Bounded a, Enum a) => a -> Bool
inRange key =
  let getBounds :: (Bounded a) => a -> (a, a)
      getBounds _ = (minBound, maxBound)
      ix = fromEnum key
  in uncurry (&&) $ (((ix >=) . fromEnum) *** ((ix <=) . fromEnum)) $
     getBounds key

makeTotalMap' :: (Bounded a, Enum a) => ((Int, Int) -> [b]) -> TotalMap a b
makeTotalMap' fn =
  let getBounds :: (Bounded a) => TotalMap a b -> (a, a)
      getBounds _ = (minBound, maxBound)
      rng = (fromEnum *** fromEnum) (getBounds tm)
      tm = TotalMap $ listArray rng $ fn rng
  in tm

-------------------------------------------------------------------------------
