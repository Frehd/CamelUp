{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}

module GameState where

import Bets
import Camels
import Data.Map.Strict (Map, foldr, fromList)
import qualified Data.Vector as Vec
import Pieces
import Plates
import Players
import Position

data GameState = GameState {pieceState :: [PieceState], diceState :: DiceState, betState :: BetState, playerState :: [Player], turn :: Player}
  deriving (Eq, Ord, Show)

newtype DiceState = DiceState {diceState :: Map Camel Bool}
  deriving (Eq, Ord, Show)

type Dice = Int

roundEnded :: GameState -> Bool
roundEnded gameState =
  Data.Map.Strict.foldr
    (&&)
    False
    ((diceState :: DiceState -> Map Camel Bool) $ (diceState :: GameState -> DiceState) gameState)

standardGameState :: GameState
standardGameState =
  GameState
    { pieceState =
        [ CamelStack $ Vec.fromList [Camel 0],
          Empty,
          Empty,
          Plate {plateKind = Positive, owner = Player {playerId = 0, money = 2, plate = Placed (PlateState Negative (Position 2)), bets = []}}
        ],
      diceState = DiceState (Data.Map.Strict.fromList [(Camel 0, False)]),
      betState = BetState (Data.Map.Strict.fromList [(Camel 0, Bet {camel = Camel 0, value = 1})]),
      playerState = [Player {playerId = 0, money = 2, plate = Unused, bets = []}],
      turn = Player {playerId = 0, money = 2, plate = Unused, bets = []}
    }

initialGameState :: Int -> GameState
initialGameState players =
  GameState
    { pieceState = replicate 10 Empty, --todo find out how many fields
      diceState = DiceState (Data.Map.Strict.fromList [(Camel i, False) | i <- [0 .. 5]]),
      betState = BetState (Data.Map.Strict.fromList [(Camel i, Bet {camel = Camel i, value = 5}) | i <- [0 .. 5]]),
      playerState = [Player {playerId = i, money = 2, plate = Unused, bets = []} | i <- [0 .. players]],
      turn = Player {playerId = 0, money = 2, plate = Unused, bets = []}
    }
