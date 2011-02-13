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

module Pylos.Modes.Gameplay
  (performTick, handleAction)
where

import Control.Concurrent (forkIO, tryPutMVar, tryTakeMVar)
import Control.Monad (unless)

import Pylos.Constants (secondsPerFrame)
import Pylos.Data.TotalMap (tmGet)
import Pylos.Game
import Pylos.State
import Pylos.Views.Board (GameplayAction(..))

-------------------------------------------------------------------------------

performTick :: GameState -> IO GameState
performTick state =
  case gsPhase state of
    CpuTurnPhase -> do
      let mvar = gsCpuMove state
      threadId <- forkIO $ do
        case chooseBestMove 2 (gsRuleset state) (gsTurn state) (gsBoard state) of
          Nothing -> error "no legal move for cpu player"
          Just (move, _) -> do
            success <- tryPutMVar mvar move
            unless success $ error "tryPutMVar failed"
      return state { gsPhase = CpuRunningPhase,
                     gsCpuThreadId = Just threadId }
    CpuRunningPhase -> do
      mbMove <- tryTakeMVar (gsCpuMove state)
      case mbMove of
        Nothing -> return state
        Just move -> return $ animateMove state move EndPhase
    AnimatePhase anim ->
      let cur' = animCurrent anim + secondsPerFrame
      in if cur' >= animMaximum anim
         then return state { gsPhase = animNextPhase anim,
                             gsBoard = animNextBoard anim }
         else let phase' = AnimatePhase anim { animCurrent = cur' }
              in return state { gsPhase = phase' }
    EndPhase ->
      let team = gsTurn state
          board = gsBoard state
      in if remainingPieces team board > 0
         then let team' = opponent team
                  phase' = case tmGet team' $ gsPlayers state of
                             CpuPlayer -> CpuTurnPhase
                             HumanPlayer -> BeginPhase
              in return state { gsTurn = team', gsPhase = phase' }
         else return $ animateDefeat state
    _ -> return state

handleAction :: GameState -> GameplayAction -> Maybe GameState
handleAction state action =
  case action of
    PlacePiece coords -> Just $
      case gsPhase state of
        BeginPhase ->
          animatePlace state coords placeTime $
          if formsFormation ruleset team board coords
          then RemovePhase $ PlaceMove coords []
          else EndPhase
        _ -> error "wrong phase"
    JumpPiece fromCoords -> Just $
      case gsPhase state of
        BeginPhase -> state { gsPhase = LandPhase fromCoords }
        _ -> error "wrong phase"
    LandPiece toCoords -> Just $
      case gsPhase state of
        LandPhase fromCoords ->
          let board' = pyramidSet Nothing fromCoords board
          in animateJump state fromCoords toCoords jumpTime $
             if formsFormation ruleset team board' toCoords
             then RemovePhase $ JumpMove fromCoords toCoords []
             else EndPhase
        _ -> error "wrong phase"
    RemovePiece coords -> Just $
      case gsPhase state of
        RemovePhase move ->
          let removals' = coords : getRemovals move
              move' = setRemovals removals' move
          in animateRemove state coords removeTime $
             if length removals' >= 2
             then EndPhase
             else RemovePhase move'
        _ -> error "wrong phase"
    UndoPiece -> Just $
      case gsPhase state of
        LandPhase _ -> state { gsPhase = BeginPhase }
        RemovePhase (PlaceMove coords []) ->
          animateRemove state coords (removeTime / 2) BeginPhase
        RemovePhase (PlaceMove coords (removal:removals)) ->
          animatePlace state removal (placeTime / 2) $
          RemovePhase (PlaceMove coords removals)
        RemovePhase (JumpMove fromCoords toCoords []) ->
          animateJump state toCoords fromCoords (jumpTime / 2) $
          LandPhase fromCoords
        RemovePhase (JumpMove from to (removal:removals)) ->
          animatePlace state removal (placeTime / 2) $
          RemovePhase (JumpMove from to removals)
        _ -> state -- ignore
    EndTurnEarly -> Just $
      case gsPhase state of
        RemovePhase (PlaceMove _ (_:_)) ->
          state { gsPhase = EndPhase }
        RemovePhase (JumpMove _ _ (_:_)) ->
          state { gsPhase = EndPhase }
        _ -> state -- ignore
    GameOver -> Nothing
  where
    board = gsBoard state
    ruleset = gsRuleset state
    team = gsTurn state

-------------------------------------------------------------------------------
-- Private functions:

placeTime :: Double
placeTime = 0.2 -- seconds

jumpTime :: Double
jumpTime = 0.3 -- seconds

removeTime :: Double
removeTime = 0.2 -- seconds

animatePlace :: GameState -> Coords -> Double -> Phase -> GameState
animatePlace state coords time phase' =
  let team = gsTurn state
      anim = Animation {
               animKind = AnimPlace coords,
               animCurrent = 0,
               animMaximum = time,
               animNextBoard = pyramidSet (Just team) coords $ gsBoard state,
               animNextPhase = phase' }
  in state { gsPhase = AnimatePhase anim }

animateJump :: GameState -> Coords -> Coords -> Double -> Phase -> GameState
animateJump state fromCoords toCoords time phase' =
  let team = gsTurn state
      board' = pyramidSet Nothing fromCoords $ gsBoard state
      anim = Animation {
               animKind = AnimJump fromCoords toCoords,
               animCurrent = 0,
               animMaximum = time,
               animNextBoard = pyramidSet (Just team) toCoords board',
               animNextPhase = phase' }
  in state { gsPhase = AnimatePhase anim, gsBoard = board' }

animateRemove :: GameState -> Coords -> Double -> Phase -> GameState
animateRemove state coords time phase' =
  let board' = pyramidSet Nothing coords $ gsBoard state
      anim = Animation {
               animKind = AnimRemove coords,
               animCurrent = 0,
               animMaximum = time,
               animNextBoard = board',
               animNextPhase = phase' }
  in state { gsPhase = AnimatePhase anim, gsBoard = board' }

animateDefeat :: GameState -> GameState
animateDefeat state =
  let board = gsBoard state
      phase = fst $ bifold (board, VictoryPhase) $
              filter (isEmpty board) $ allCoords board
  in state { gsTurn = team, gsPhase = phase }
  where
    team = opponent (gsTurn state)
    isEmpty board coords = pyramidGet coords board == Nothing
    bifold :: (Board, Phase) -> [Coords] -> (Phase, Board)
    bifold (board, phase) [] = (phase, board)
    bifold (board, phase) (c:cs) =
      let (phase', board'') = bifold (board', phase) cs
          board' = pyramidSet (Just team) c board
          phase'' = AnimatePhase $ Animation {
                      animKind = AnimPlace c,
                      animCurrent = 0,
                      animMaximum = placeTime,
                      animNextBoard = board',
                      animNextPhase = phase' }
      in (phase'', board'')

animateMove :: GameState -> Move -> Phase -> GameState
animateMove state move nextPhase =
  case move of
    PlaceMove coords removals ->
      let board' = pyramidSet (Just team) coords $ gsBoard state
      in animatePlace state coords placeTime $
         animateRemovals board' removals
    JumpMove fromCoords toCoords removals ->
      let board' = pyramidSet Nothing fromCoords $
                   pyramidSet (Just team) toCoords $ gsBoard state
      in animateJump state fromCoords toCoords jumpTime $
         animateRemovals board' removals
  where
    team = gsTurn state
    animateRemovals board removals = fst $ bifold (board, nextPhase) removals
    bifold :: (Board, Phase) -> [Coords] -> (Phase, Board)
    bifold (board, phase) [] = (phase, board)
    bifold (board, phase) (c:cs) =
      let (phase', board'') = bifold (board', phase) cs
          board' = pyramidSet Nothing c board
          phase'' = AnimatePhase $ Animation {
                      animKind = AnimRemove c,
                      animCurrent = 0,
                      animMaximum = removeTime,
                      animNextBoard = board',
                      animNextPhase = phase' }
      in (phase'', board'')

-------------------------------------------------------------------------------
