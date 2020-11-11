{-# LANGUAGE DuplicateRecordFields #-}

module GameState where

import Bets
import {-# SOURCE #-} Camels (Camel (..))
import Data.List (findIndex)
import Data.Map.Strict (fromList)
import qualified Data.Vector as Vec
import {-# SOURCE #-} Dice
import Pieces
import Plates
import Players

data GameState = GameState {pieceState :: [PieceState], diceState :: DiceState, betState :: BetState, playerState :: [Player], turn :: PlayerId}
  deriving (Eq, Ord, Show)

nextTurn :: GameState -> GameState
nextTurn gameState = gameState {turn = nextPlayer}
  where
    nextPlayer = case currentPlayerIndex of
      Just a | a + 1 < length (playerState gameState) -> playerId $ playerState gameState !! (a + 1)
      Just _ -> playerId $ head (playerState gameState)
      where
        currentPlayerIndex = findIndex (\player -> turn gameState == playerId player) (playerState gameState)

standardGameState :: GameState
standardGameState =
  GameState
    { pieceState =
        [ CamelStack $ Vec.fromList [Camel 0],
          Empty,
          Empty,
          Plate {plateKind = Positive, owner = PlayerId 0}
        ],
      diceState = DiceState (Data.Map.Strict.fromList [(Camel 0, False)]),
      betState = BetState (Data.Map.Strict.fromList [(Camel 0, Bet {camel = Camel 0, value = 1})]),
      playerState = [Player {playerId = PlayerId 0, money = 2, plate = Unused, bets = []}],
      turn = PlayerId 0
    }

initialGameState :: Int -> GameState
initialGameState players =
  GameState
    { pieceState = CamelStack (Vec.fromList [Camel i | i <- [0 .. 5]]) : replicate 10 Empty, --todo find out how many fields
      diceState = DiceState (Data.Map.Strict.fromList [(Camel i, False) | i <- [0 .. 5]]),
      betState = BetState (Data.Map.Strict.fromList [(Camel i, Bet {camel = Camel i, value = 5}) | i <- [0 .. 5]]),
      playerState = [Player {playerId = PlayerId i, money = 2, plate = Unused, bets = []} | i <- [0 .. players]],
      turn = PlayerId 0
    }
