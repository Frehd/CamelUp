module Moves (Move (..), execMove, possibleMoves) where

import Bets
import Camels
import Data.Set (Set, empty, fromList, insert, map, singleton, toList, union)
import Dice
import GameState
import Plates

data Move = ThrowDice | RoundBet Bet | GameBet Bet | LayPlate PlateState
  deriving (Eq, Ord, Show)

throwDice :: GameState -> [GameState]
throwDice gameState =
  [ gameState {pieceState = moveCamel camel spaces (GameState.pieceState gameState), GameState.diceState = markDiceThrown camel (GameState.diceState gameState)}
    | camel <- Data.Set.toList $ possibleDice gameState,
      spaces <- [1 .. 3]
  ]

execMove :: Move -> GameState -> Set GameState
execMove (LayPlate plateState) gameState = Data.Set.singleton gameState {pieceState = addPlate (pieceState gameState) plateState (turn gameState)}
execMove (RoundBet bet) gameState = Data.Set.singleton (gameState :: GameState) {GameState.betState = takeBet bet (GameState.betState gameState)}
execMove ThrowDice gameState = Data.Set.fromList $ throwDice gameState
execMove _ _ = Data.Set.empty

possibleMoves :: GameState -> Set Move
possibleMoves gameState =
  Data.Set.insert
    ThrowDice
    ( Data.Set.union
        (Data.Set.map LayPlate $ possiblePlates gameState)
        (Data.Set.map RoundBet $ possibleBets gameState)
    )
