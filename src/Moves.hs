module Moves (Move (..), execMove, possibleMoves) where

import Bets
import Camels
import Control.Lens.Combinators (element)
import Control.Lens.Operators
import Data.Set (Set, empty, fromList, insert, map, toList, union)
import Dice
import GameState
import Plates
import Players

data Move = ThrowDice | RoundBet Bet | GameBet Bet | LayPlate PlateState
  deriving (Eq, Ord, Show)

payDiceMoney :: GameState -> GameState
payDiceMoney gameState = gameState {GameState.playerState = newPlayerState}
  where
    playerIndex = getPlayerIndex (GameState.turn gameState) (GameState.playerState gameState)
    newPlayerState = (element playerIndex .~ newPlayer) (GameState.playerState gameState)
    newPlayer = addMoney 1 $ getPlayer (GameState.turn gameState) (GameState.playerState gameState)

throwDice :: GameState -> [GameState]
throwDice gameState =
  [ payDiceMoney $ moveCamel camel spaces gameState {GameState.diceState = markDiceThrown camel (GameState.diceState gameState)}
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
