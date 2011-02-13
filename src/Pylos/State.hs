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

module Pylos.State where

import Control.Concurrent (MVar, ThreadId, newEmptyMVar)

import Pylos.Data.TotalMap (TotalMap, makeTotalMap)
import Pylos.Game

-------------------------------------------------------------------------------

data GameState = GameState
  { gsRuleset :: Ruleset,
    gsBoard :: Board,
    gsTurn :: Team,
    gsPhase :: Phase,
    gsPlayers :: TotalMap Team Player,
    gsCpuThreadId :: Maybe ThreadId,
    gsCpuMove :: MVar Move }

data NewGameSpec = NewGameSpec
  { ngRuleset :: Ruleset,
    ngBoardSize :: Int,
    ngWhitePlayer :: Player,
    ngBlackPlayer :: Player }

newGameState :: NewGameSpec -> IO GameState
newGameState spec = do
  cpuMove <- newEmptyMVar
  return GameState
    { gsRuleset = ngRuleset spec,
      gsBoard = emptyBoard (ngBoardSize spec),
      gsTurn = WhiteTeam,
      gsPhase = case ngWhitePlayer spec of
                  HumanPlayer -> BeginPhase
                  CpuPlayer -> CpuTurnPhase,
      gsPlayers = makeTotalMap $
                  \team -> case team of WhiteTeam -> ngWhitePlayer spec
                                        BlackTeam -> ngBlackPlayer spec,
      gsCpuThreadId = Nothing,
      gsCpuMove = cpuMove }

-------------------------------------------------------------------------------

data Player = CpuPlayer | HumanPlayer
  deriving Eq

-------------------------------------------------------------------------------

data Phase = CpuTurnPhase -- start of computer turn
           | CpuRunningPhase -- during computer turn
           | BeginPhase -- human turn: place or jump piece
           | LandPhase Coords -- human turn: land a jumped piece
           | RemovePhase Move -- human turn: remove a piece
           | AnimatePhase Animation -- animation in progress
           | EndPhase -- turn is now finished
           | VictoryPhase -- current player has won

data Animation = Animation
  { animKind :: AnimKind,
    animCurrent :: Double,
    animMaximum :: Double,
    animNextBoard :: Board,
    animNextPhase :: Phase }

data AnimKind = AnimPlace Coords
              | AnimJump Coords Coords
              | AnimRemove Coords

-------------------------------------------------------------------------------
