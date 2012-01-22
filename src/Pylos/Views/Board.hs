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

module Pylos.Views.Board
  (GameplayAction(..), newGameplayView)
where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (when)
import Data.Ix (range)
import Data.List (find)
import Data.Traversable (traverse)

import Pylos.Constants (screenWidth)
import Pylos.Data.Color
import Pylos.Data.Point
import Pylos.Data.TotalMap (makeTotalMap, tmGet)
import Pylos.Draw
import Pylos.Event
import Pylos.Game
import Pylos.State
import Pylos.Utility (maybeM)
import Pylos.Views
import Pylos.Views.Widgets (drawBevelRect, newTextButton)

-------------------------------------------------------------------------------

data GameplayAction = PlacePiece Coords
                    | JumpPiece Coords
                    | LandPiece Coords
                    | RemovePiece Coords
                    | UndoPiece
                    | EndTurnEarly
                    | GameOver

newGameplayView :: (MonadDraw m) => m (View GameState GameplayAction)
newGameplayView = do
  background <- loadSprite "board.png"
  let backgroundView =
        inertView $ const $ blitTopleft background (pZero :: IPoint)
  -- Board:
  let boardSide = 332
  let boardTop = 52
  boardView <- fmap (subView_ (Rect ((screenWidth - boardSide) `div` 2)
                                    boardTop boardSide boardSide)) newBoardView
  -- Player views:
  let pvWidth = 120
  let pvMargin = 15
  let pvHeight = 105
  whiteView <- fmap (subView_ (Rect pvMargin 130 pvWidth pvHeight)) $
               newPlayerView WhiteTeam
  blackView <- fmap (subView_ (Rect (screenWidth - pvMargin - pvWidth) 130
                                    pvWidth pvHeight)) $
               newPlayerView BlackTeam
  -- Buttons:
  let buttonTop = 415
  let canUndo state =
        case gsPhase state of
          LandPhase _ -> True
          RemovePhase _ -> True
          AnimatePhase (Animation { animNextPhase = RemovePhase move }) ->
            not $ null $ getRemovals move
          _ -> False
  undoButton <- fmap (subView_ (Rect 375 buttonTop 90 30) .
                      viewMap (const "Undo" &&& canUndo) id) $
                newTextButton Nothing UndoPiece
  let canEndEarly state = case gsPhase state of
                            RemovePhase move -> not $ null $ getRemovals move
                            _ -> False
  doneButton <- fmap (subView_ (Rect 275 buttonTop 90 30) .
                      viewMap (const "Done" &&& canEndEarly) id) $
                newTextButton Nothing EndTurnEarly
  let endGameText state = case gsPhase state of
                            VictoryPhase -> "Game Over"
                            _ -> "Resign"
  endGameButton <- fmap (subView_ (Rect 175 buttonTop 90 30) .
                         viewMap (endGameText &&& const True) id) $
                   newTextButton Nothing GameOver
  return $ compoundView [backgroundView, boardView, whiteView, blackView,
                         undoButton, doneButton, endGameButton]

-------------------------------------------------------------------------------

newPlayerView :: (MonadDraw m) => Team -> m (View GameState a)
newPlayerView team = do
  font1 <- loadFont "caligula.ttf" 36
  font2 <- loadFont "caligula.ttf" 24
  let
    paint :: GameState -> Paint ()
    paint state = do
      cx <- fmap (`div` 2) canvasWidth
      let kind = case tmGet team $ gsPlayers state of
                   HumanPlayer -> "Human"
                   CpuPlayer -> "CPU"
      drawText font2 whiteColor (LocCenter $ Point cx 20) kind
      let name = case team of { BlackTeam -> "Black"; WhiteTeam -> "White" }
      drawText font1 whiteColor (LocCenter $ Point cx 50) name
      let remain = "Pieces: " ++ show (remainingPieces team $ gsBoard state)
      drawText font2 whiteColor (LocCenter $ Point cx 85) remain
      rect <- canvasRect
      when (gsTurn state == team) $ do
        drawBevelRect (Tint 255 230 100 255) 5 rect
  return $ inertView paint

-------------------------------------------------------------------------------

newBoardView :: (MonadDraw m) => m (View GameState GameplayAction)
newBoardView = do
  pieceSprites <- let piecePath BlackTeam = "black-piece.png"
                      piecePath WhiteTeam = "white-piece.png"
                  in traverse loadSprite $ makeTotalMap piecePath
  let

    paint :: GameState -> Paint ()
    paint state = do
      -- Draw the base of the board.
      size <- canvasSize
      let drawBase = tintOval (Tint 255 255 255 10)
      let shrink rect = let w = rectW rect / 5
                            h = rectH rect / 5
                        in adjustRect w h w h rect
      mapM_ drawBase $ map shrink $ baseRects size (gsBoard state)
      -- Draw pieces, including hilights (if any):
      let getHilights phase =
            case phase of
              LandPhase fromCoords -> [(fromCoords, jumpTint)]
              RemovePhase move ->
                case move of
                  PlaceMove coords removals ->
                    (coords, placeTint) : map (flip (,) removeTint) removals
                  JumpMove fromCoords toCoords removals ->
                    (fromCoords, jumpTint) : (toCoords, placeTint) :
                    map (flip (,) removeTint) removals
              AnimatePhase anim -> getHilights (animNextPhase anim)
              _ -> []
            where placeTint = Tint 0 255 0 128
                  jumpTint = Tint 255 0 0 128
                  removeTint = Tint 0 0 255 128
      let hilights = getHilights (gsPhase state)
      let board' = case gsPhase state of
                     RemovePhase move ->
                       applyMove (gsTurn state) move (gsBoard state)
                     _ -> gsBoard state
      let drawPiece rect team = blitStretch (tmGet team pieceSprites) rect
      let drawHilight rect tint = drawOval tint rect
      let drawCell (coords, rect) = do
            maybeM (pyramidGet coords board') (drawPiece rect)
            maybeM (lookup coords hilights) (drawHilight rect)
      mapM_ drawCell $ coordsAndRects size board'
      -- Draw the current animation, if any:
      case gsPhase state of
        AnimatePhase anim -> do
          let mkCenter (Rect x y w h) = Rect (x + w/2) (y + h/2) 0 0
          let transition (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) = do
                let tau = animCurrent anim / animMaximum anim
                let linear start end = start + tau * (end - start)
                let w' = linear w1 w2
                    h' = linear h1 h2
                let x' = linear (x1 + w1/2) (x2 + w2/2) - w'/2
                    y' = linear (y1 + h1/2) (y2 + h2/2) - h'/2
                let rect' = Rect x' y' w' h'
                drawPiece rect' (gsTurn state)
          case animKind anim of
            AnimPlace coords -> do
              let rect = coordsRect size board' coords
              transition (mkCenter rect) rect
            AnimJump fromCoords toCoords -> do
              transition (coordsRect size board' fromCoords)
                         (coordsRect size board' toCoords)
            AnimRemove coords -> do
              let rect = coordsRect size board' coords
              transition rect (mkCenter rect)
        _ -> return ()

    handler :: GameState -> IRect -> Event -> Draw (Maybe GameplayAction)
    handler state rect (EvMouseDown pt) =
      case gsPhase state of
        CpuTurnPhase -> ignore
        CpuRunningPhase -> ignore
        BeginPhase ->
          return $ (fmap PlacePiece $ ptCoords $ canPlacePiece board) <|>
                   (fmap JumpPiece $ ptCoords $
                    ownedAnd $ canRemovePiece board)
        LandPhase fromCoords ->
          return $ fmap LandPiece $ ptCoords $ canJumpPiece board fromCoords
        RemovePhase move ->
          return $ fmap RemovePiece $ ptCoords $ ownedAnd $ canRemovePiece $
          applyMove (gsTurn state) move board
        AnimatePhase _ -> ignore
        EndPhase -> ignore
        VictoryPhase ->
          if rectContains rect pt then return (Just GameOver) else ignore
      where
        board = gsBoard state
        team = gsTurn state
        ownedAnd :: (Coords -> Bool) -> (Coords -> Bool)
        ownedAnd fn coords = pyramidGet coords board == Just team && fn coords
        ptCoords :: (Coords -> Bool) -> Maybe Coords
        ptCoords fn = find fn $ reverse $ map fst $
                      filter (ovalContains (fmap fromIntegral $
                                            pt `pSub` rectTopleft rect) .
                              snd) $
                      coordsAndRects (rectSize rect) board
    handler _ _ (EvKeyDown KeyZ [KeyModCmd] _) = return (Just UndoPiece)
    handler state _ (EvKeyDown KeySpace _ _) =
      case gsPhase state of
        VictoryPhase -> return (Just GameOver)
        _ -> return (Just EndTurnEarly)
    handler _ _ _ = ignore
    ignore = return Nothing

    ovalContains :: DPoint -> DRect -> Bool
    ovalContains (Point px py) (Rect x y w h) =
      let hRad = w / 2
          vRad = h / 2
          dx = (px - (x + hRad)) / hRad
          dy = (py - (y + vRad)) / vRad
      in dx*dx + dy*dy <= 1

    coordsRect :: (Int, Int) -> Board -> Coords -> DRect
    coordsRect size board coords = withMkRect size board ($ coords)

    coordsAndRects :: (Int, Int) -> Board -> [(Coords, DRect)]
    coordsAndRects size board =
      withMkRect size board (\mkRect -> map (id &&& mkRect) $ allCoords board)

    baseRects :: (Int, Int) -> Board -> [DRect]
    baseRects size board = withMkRect size board $ flip map cs where
      cs = [Coords lev row col | (row, col) <- range ((1, 1), (lev, lev))]
      lev = pyramidLevels board

    withMkRect :: (Int, Int) -> Board -> ((Coords -> DRect) -> a) -> a
    withMkRect (width, height) board fn =
      let gap = 5 :: Double
          fullSide = fromIntegral (min width height)
          levels = fromIntegral (pyramidLevels board)
          baseSide = (fullSide - gap * (levels - 1)) / levels
          stride = baseSide + gap
          mkRect (Coords level row col) =
            let level' = fromIntegral level
                side = baseSide * (level' + 4 * levels) / (5 * levels)
                offset = (fullSide - side - stride * (level' - 1)) / 2
            in Rect (offset + stride * fromIntegral (col - 1))
                    (offset + stride * fromIntegral (row - 1)) side side
      in fn mkRect

  return $ View paint handler

-------------------------------------------------------------------------------
