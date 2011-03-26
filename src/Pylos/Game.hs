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

module Pylos.Game where

import Control.Applicative (Applicative)
import Control.Arrow ((&&&))
import Control.Exception (assert)
import Control.Monad (guard)
import Data.Array ((!), (//), Array, bounds, listArray, range)
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe, isJust, isNothing)

import Pylos.Utility (maximumKey)

-------------------------------------------------------------------------------

data Ruleset = SimpleRules | NormalRules | ExpertRules
  deriving (Eq, Show)

-------------------------------------------------------------------------------

data Team = BlackTeam | WhiteTeam
  deriving (Bounded, Enum, Eq, Show)

opponent :: Team -> Team
opponent BlackTeam = WhiteTeam
opponent WhiteTeam = BlackTeam

-------------------------------------------------------------------------------

data Coords = Coords
  { coordsLevel :: !Int,
    coordsRow :: !Int,
    coordsCol :: !Int }
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------

newtype Pyramid a = Pyramid (Array Int (Array (Int, Int) a))

instance Functor Pyramid where
  fmap f (Pyramid arr) = Pyramid (fmap (fmap f) arr)

instance Fold.Foldable Pyramid where
  foldMap fn (Pyramid arr) = Fold.foldMap (Fold.foldMap fn) arr

pyramidLevels :: Pyramid a -> Int
pyramidLevels (Pyramid arr) = snd (bounds arr)

makePyramid :: Int -> a -> Pyramid a
makePyramid levels value =
  let level size = listArray ((1, 1), (size, size)) (repeat value)
  in Pyramid (listArray (1, levels) $ map level [1..levels])

pyramidGet :: Coords -> Pyramid a -> a
pyramidGet (Coords level row col) (Pyramid arr) =
  assert (level >= 1) $
  assert (level <= snd (bounds arr)) $
  assert (row >= 1) $
  assert (col >= 1) $
  assert (row <= level) $
  assert (col <= level) $
  arr ! level ! (row, col)

pyramidSet :: a -> Coords -> Pyramid a -> Pyramid a
pyramidSet value (Coords level row col) (Pyramid arr) =
  assert (level >= 1) $
  assert (level <= snd (bounds arr)) $
  assert (row >= 1) $
  assert (col >= 1) $
  assert (row <= level) $
  assert (col <= level) $
  Pyramid $ arr // [(level, (arr ! level) // [((row, col), value)])]

allCoords :: Pyramid a -> [Coords]
allCoords (Pyramid arr) = concatMap fn $ reverse $ uncurry enumFromTo $
                          bounds arr
  where fn level = map mkCoords $ range ((1, 1), (level, level))
          where mkCoords (row, col) = Coords level row col

traverseWithCoords_ :: (Applicative f) => (Coords -> a -> f b)
                    -> Pyramid a -> f ()
traverseWithCoords_ fn pyr =
  Fold.traverse_ (uncurry fn) $ map getPair $ allCoords pyr where
    getPair coords = (coords, pyramidGet coords pyr)

-------------------------------------------------------------------------------

type Board = Pyramid (Maybe Team)

emptyBoard :: Int -> Board
emptyBoard = flip makePyramid Nothing

demoBoard :: Board
demoBoard = pyramidSet (Just BlackTeam) (Coords 4 1 1) $
            pyramidSet (Just BlackTeam) (Coords 3 1 2) $
            pyramidSet (Just WhiteTeam) (Coords 4 2 2) $
            pyramidSet (Just WhiteTeam) (Coords 4 1 2) $
            pyramidSet (Just BlackTeam) (Coords 4 2 3) $
            pyramidSet (Just WhiteTeam) (Coords 4 1 3) $
            makePyramid 4 Nothing

remainingPieces :: Team -> Board -> Int
remainingPieces team board =
  (sum $ map (\x -> x * x) [1..pyramidLevels board]) `div` 2 -
  (Fold.sum $ fmap (\slot -> if slot == Just team then 1 else 0) $ board)

canPlacePiece :: Board -> Coords -> Bool
canPlacePiece board coords@(Coords level row col) =
  isNothing (pyramidGet coords board) &&
  (level == pyramidLevels board ||
   isJust (pyramidGet (Coords (level + 1) row col) board) &&
   isJust (pyramidGet (Coords (level + 1) (row + 1) col) board) &&
   isJust (pyramidGet (Coords (level + 1) row (col + 1)) board) &&
   isJust (pyramidGet (Coords (level + 1) (row + 1) (col + 1)) board))

canRemovePiece :: Board -> Coords -> Bool
canRemovePiece board coords@(Coords level row col) =
  isJust (get coords) &&
  (row <= 1 ||
   (col <= 1 || isNothing (get (Coords (level - 1) (row - 1) (col - 1)))) &&
   (col >= level || isNothing (get (Coords (level - 1) (row - 1) col)))) &&
  (row >= level ||
   (col <= 1 || isNothing (get (Coords (level - 1) row (col - 1)))) &&
   (col >= level || isNothing (get (Coords (level - 1) row col))))
  where get c = pyramidGet c board

canJumpPiece :: Board -> Coords -> Coords -> Bool
canJumpPiece board fromCoords toCoords =
  coordsLevel fromCoords > coordsLevel toCoords &&
  canRemovePiece board fromCoords &&
  canPlacePiece (pyramidSet Nothing fromCoords board) toCoords

formsFormation :: Ruleset -> Team -> Board -> Coords -> Bool
formsFormation SimpleRules _ _ _ = False
formsFormation NormalRules team board (Coords level row col) =
  (row > 1 &&
   ((col > 1 &&
     matches (Coords level (row - 1) col) &&
     matches (Coords level (row - 1) (col - 1)) &&
     matches (Coords level row (col - 1))) ||
    (col < level &&
     matches (Coords level (row - 1) col) &&
     matches (Coords level (row - 1) (col + 1)) &&
     matches (Coords level row (col + 1))))) ||
  (row < level &&
   ((col > 1 &&
     matches (Coords level (row + 1) col) &&
     matches (Coords level (row + 1) (col - 1)) &&
     matches (Coords level row (col - 1))) ||
    (col < level &&
     matches (Coords level (row + 1) col) &&
     matches (Coords level (row + 1) (col + 1)) &&
     matches (Coords level row (col + 1)))))
  where matches c = pyramidGet c board == Just team
formsFormation ExpertRules team board coords@(Coords level row col) =
  formsFormation NormalRules team board coords ||
  level > 2 &&
  (all (\row' -> row' == row || matches (Coords level row' col)) [1..level] ||
   all (\col' -> col' == col || matches (Coords level row col')) [1..level])
  where matches c = pyramidGet c board == Just team

-------------------------------------------------------------------------------

data Move = PlaceMove !Coords [Coords]
          | JumpMove !Coords !Coords [Coords]
  deriving (Eq, Show)

getRemovals :: Move -> [Coords]
getRemovals (PlaceMove _ removals) = removals
getRemovals (JumpMove _ _ removals) = removals

setRemovals :: [Coords] -> Move -> Move
setRemovals removals (PlaceMove c _) = PlaceMove c removals
setRemovals removals (JumpMove c1 c2 _) = JumpMove c1 c2 removals

applyRemovals :: [Coords] -> Board -> Board
applyRemovals cs = foldr (.) id $ map (pyramidSet Nothing) cs

applyMove :: Team -> Move -> Board -> Board
applyMove team (PlaceMove coords removals) board =
  applyRemovals removals $ pyramidSet (Just team) coords $ board
applyMove team (JumpMove from to removals) board =
  applyRemovals removals $ pyramidSet (Just team) to $
  pyramidSet Nothing from $ board

isLegalMove :: Ruleset -> Team -> Board -> Move -> Bool
isLegalMove rules team board (PlaceMove coords remove) =
  fromMaybe False $ do
    guard (canPlacePiece board coords)
    guard (null remove || formsFormation rules team board coords)
    let board' = pyramidSet (Just team) coords board
    return $ isLegalRemoval board' remove
isLegalMove rules team board (JumpMove coords1 coords2 remove) =
  fromMaybe False $ do
    guard (coordsLevel coords1 > coordsLevel coords2)
    guard (pyramidGet coords1 board == Just team)
    guard (canRemovePiece board coords1)
    let board' = pyramidSet Nothing coords1 board
    return $ isLegalMove rules team board' (PlaceMove coords2 remove)

isLegalRemoval :: Board -> [Coords] -> Bool
isLegalRemoval board remove = snd $ foldr fn (board, True) remove where
  fn _ x@(_, False) = x
  fn coords (board', True) =
    if canRemovePiece board' coords
    then (pyramidSet Nothing coords board', True)
    else (board', False)

allLegalMoves :: Ruleset -> Team -> Board -> [Move]
allLegalMoves ruleset team board = allJumpMoves ++ allPlaceMoves where
  allJumpMoves =
    concatMap jumpMoves $ filter (canRemovePiece board) $
    filter (owned board) $ allCoords board
  jumpMoves fromCoords =
    concatMap jumpMoves' $ filter (canPlacePiece board') $
    filter (\c -> coordsLevel c < coordsLevel fromCoords) $ allCoords board'
      where
        board' = pyramidSet Nothing fromCoords board
        jumpMoves' toCoords =
          if formsFormation ruleset team board toCoords
          then removalMoves (JumpMove fromCoords toCoords) $
               pyramidSet (Just team) toCoords board'
          else [JumpMove fromCoords toCoords []]
  allPlaceMoves =
    concatMap placeMoves $ filter (canPlacePiece board) $ allCoords board
  placeMoves coords =
    if formsFormation ruleset team board coords
    then removalMoves (PlaceMove coords) $ pyramidSet (Just team) coords board
    else [PlaceMove coords []]
  removalMoves moveFn board' =
    concatMap removalMoves' $ filter (canRemovePiece board') $
    filter (owned board') $ allCoords board'
      where
        removalMoves' coords1 =
          (moveFn [coords1] :) $
          map removalMove $ filter (canRemovePiece board'') $
          filter (owned board'') $ allCoords board''
            where
              board'' = pyramidSet Nothing coords1 board'
              removalMove coords2 = moveFn [coords2, coords1]
  owned board' coords = pyramidGet coords board' == Just team

-------------------------------------------------------------------------------

chooseBestMove :: Int -> Ruleset -> Team -> Board -> Maybe (Move, Double)
chooseBestMove depth rules team board =
  if null moves then Nothing else Just $
    if depth <= 0 then maximumKey snd $ map (id &&& moveScore board) moves
    else maximumKey snd $ map (id &&& moveScore') moves
  where
    moves = allLegalMoves rules team board
    moveScore board' move = boardScore rules team $ applyMove team move board'
    moveScore' move = recip $
      case chooseBestMove (depth - 1) rules (opponent team)
             (applyMove team move board) of
        Nothing -> 0
        Just (_, score) -> score

boardScore :: Ruleset -> Team -> Board -> Double
boardScore _ team board =
  fromIntegral (remainingPieces team board) /
  fromIntegral (remainingPieces (opponent team) board)

-------------------------------------------------------------------------------
