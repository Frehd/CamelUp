module Moves (Move (..), execMove, possibleMoves) where

import Bets
import Camels
import Data.Set (Set, empty, fromList, insert, map, toList, union)
import Dice
import GameState
import Plates

data Move = ThrowDice | RoundBet Bet | GameBet Bet | LayPlate PlateState
  deriving (Eq, Ord, Show)

throwDice :: GameState -> [GameState]
throwDice gameState =
  [ moveCamel camel spaces gameState {GameState.diceState = markDiceThrown camel (GameState.diceState gameState)}
    | camel <- Data.Set.toList $ possibleDice gameState,
      spaces <- [1 .. 3]
  ]

execMove :: Move -> GameState -> Either (Set GameState) GameState
execMove (LayPlate plateState) gameState = Right $ nextTurn $ gameState {pieceState = addPlate (pieceState gameState) plateState (turn gameState)}
execMove (RoundBet bet) gameState = Right $ nextTurn $ (gameState :: GameState) {GameState.betState = takeBet bet (GameState.betState gameState)}
execMove ThrowDice gameState = Left $ Data.Set.fromList $ Prelude.map nextTurn $ throwDice gameState
execMove _ _ = Left Data.Set.empty

possibleMoves :: GameState -> Set Move
possibleMoves gameState =
  Data.Set.insert
    ThrowDice
    ( Data.Set.union
        (Data.Set.map LayPlate $ possiblePlates gameState)
        (Data.Set.map RoundBet $ possibleBets gameState)
    )
