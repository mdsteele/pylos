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

{-# LANGUAGE ForeignFunctionInterface, MagicHash, UnboxedTuples #-}

module Pylos.Utility
  (-- * List functions
   firstJust, minimumKey, maximumKey, sortKey,
   -- * Function combinators
   flip3, flip4, maybeM, untilM,
   -- * Random
   split3, multiSplit, randomElem,
   -- * IO
   delayFinalizers)
where

import Control.Arrow ((&&&))
import Control.Monad (msum)
import Data.List (maximumBy, minimumBy, sortBy, unfoldr)
import Data.Ord (comparing)
import qualified GHC.IO
import qualified GHC.Prim
import System.Random (RandomGen, randomR, split)

-------------------------------------------------------------------------------
-- List functions:

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust = (msum .) . map

-- | Apply the given function to all items in the list, and return the list
--   item that yields the minimum result.
minimumKey :: (Ord b) => (a -> b) -> [a] -> a
minimumKey fn = snd . minimumBy (comparing fst) . map (fn &&& id)

-- | Apply the given function to all items in the list, and return the list
--   item that yields the maximum result.
maximumKey :: (Ord b) => (a -> b) -> [a] -> a
maximumKey fn = snd . maximumBy (comparing fst) . map (fn &&& id)

-- | Apply the given function to all items in the list, and sort the items in
--   the list by comparing the results of the function.
sortKey :: (Ord b) => (a -> b) -> [a] -> [a]
sortKey fn = map snd . sortBy (comparing fst) . map (fn &&& id)

-------------------------------------------------------------------------------
-- Function combinators:

flip3 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip3 f b c a = f a b c

flip4 :: (a -> b -> c -> d -> e) -> b -> c -> d -> a -> e
flip4 f b c d a = f a b c d

maybeM :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
maybeM = flip $ maybe $ return ()

untilM :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
untilM value done update action =
  if done value then return ()
  else action value >> untilM (update value) done update action

-------------------------------------------------------------------------------
-- Random numbers:

-- | Split a random-number generator into three independent generators.
split3 :: (RandomGen a) => a -> (a, a, a)
split3 gen = let (gen1, gen') = split gen
                 (gen2, gen3) = split gen'
             in (gen1, gen2, gen3)

-- | Split a random-number generator into an infinite list of independent
--   generators.
multiSplit :: (RandomGen a) => a -> [a]
multiSplit = unfoldr (Just . split)

-- | Choose a random element from a (non-empty) list.
randomElem :: (RandomGen a) => a -> [b] -> (b, a)
randomElem gen list = if null list then error "randomElem: empty list"
                      else let (index, gen') = randomR (0, length list - 1) gen
                           in (list !! index, gen')

-------------------------------------------------------------------------------
-- IO functions:

-- | Perform an IO action, and ensure that System.Mem.Weak finalizers do not
--   run on the given object before the action has completed.
delayFinalizers :: a -> IO b -> IO b
delayFinalizers value action = do
  result <- action
  touch value
  return result

-- | Force an object not to have been garbage collected before this point in
--   the computation.
touch :: a -> IO ()
touch x =  -- So, I _think_ this works, but I'm not totally sure.
  GHC.IO.IO (\s -> case GHC.Prim.touch# x s of { s' -> (# s', () #) })

-------------------------------------------------------------------------------
