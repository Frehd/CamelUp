module Plates (PlayerPlateState (..), PlateState (..), PlateKind (..), addPlate, possiblePlates) where

import Camels
import Control.Lens.Combinators (element)
import Control.Lens.Operators
import Data.Set (Set, empty, foldl, fromList, insert, union)
import {-# SOURCE #-} GameState
import {-# SOURCE #-} Pieces
import Players
import Position
import Relude.List

data PlayerPlateState = Unused | Placed PlateState
  deriving (Eq, Ord, Show)

data PlateState = PlateState PlateKind Position
  deriving (Eq, Ord, Show)

data PlateKind = Positive | Negative
  deriving (Eq, Ord, Show)

addPlate :: [PieceState] -> PlateState -> PlayerId -> [PieceState]
addPlate pieceStates (PlateState plateKind' pos) playerId = (element (coordsOfPos pos) .~ (Plate {plateKind = plateKind', owner = playerId})) pieceStates

possiblePlatePositions :: GameState -> Set Position
possiblePlatePositions gameState = recursivePlatePositions (Position 0) gameState Data.Set.empty

recursivePlatePositions :: Position -> GameState -> Set Position -> Set Position
recursivePlatePositions (Position pos) GameState {pieceState = pieces} result | pos == length pieces = result
recursivePlatePositions (Position pos) gameState result =
  if platePlaceable (Position pos) gameState
    then
      Data.Set.union (Data.Set.insert (Position pos) result) $
        recursivePlatePositions (Position (pos + 1)) gameState result
    else
      Data.Set.union result $
        recursivePlatePositions (Position (pos + 1)) gameState result

platePlaceable :: Position -> GameState -> Bool
platePlaceable (Position pos) GameState {pieceState = pieces} | pieces !! pos == Empty && pos > lastCamelIndex pieces =
  case pieces !!? (pos -1) of
    Just (Plate _ _) -> False
    _ ->
      case pieces !!? (pos + 1) of
        Just (Plate _ _) -> False
        _ -> True
platePlaceable _ _ = False

possiblePlates :: GameState -> Set PlateState
possiblePlates gameState = case plate $ getPlayer (turn gameState) (playerState gameState) of
  Unused ->
    Data.Set.foldl
      (\set pos -> Data.Set.union (Data.Set.fromList [PlateState Positive pos, PlateState Negative pos]) set)
      Data.Set.empty
      $ possiblePlatePositions gameState
  Placed _ -> Data.Set.empty
