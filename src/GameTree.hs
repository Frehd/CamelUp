module GameTree where

import Data.Set
import GameState
import Moves

type Depth = Int

type Value = Int

data GameStage = GameStage (Set MoveStage) | Evaluation Value
  deriving (Eq, Ord, Show)

newtype MoveStage
  = MoveStage (Either (Set GameStage) GameStage)
  deriving (Eq, Ord, Show)

makeMoveStage :: Move -> GameState -> Depth -> MoveStage
makeMoveStage move gameState depth = MoveStage gameStage
  where
    gameStage = case execMove move gameState of
      Left setGameState -> Left $ Data.Set.map (`makeGameStage` depth) setGameState
      Right singleGameState -> Right $ makeGameStage singleGameState depth

makeGameStage :: GameState -> Depth -> GameStage
makeGameStage gameState depth | depth > 0 = GameStage (Data.Set.map (\move -> makeMoveStage move gameState (depth -1)) (possibleMoves gameState))
makeGameStage gameState _ = Evaluation 0
