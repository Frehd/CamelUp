{-# LANGUAGE DuplicateRecordFields #-}
module GameState where
import Data.Map.Strict (Map)
import Pieces
import Camels
import Players
import {-# SOURCE #-} Bets

data GameState = GameState {pieceState :: [PieceState], diceState :: DiceState, betState :: BetState, playerState :: [Player], turn :: Player}
instance Show GameState
instance Eq GameState
instance Ord GameState

newtype DiceState = DiceState {diceState :: Map Camel Bool}
instance Show DiceState
instance Eq DiceState
instance Ord DiceState