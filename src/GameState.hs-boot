{-# LANGUAGE DuplicateRecordFields #-}

module GameState where

import {-# SOURCE #-} Bets
import {-# SOURCE #-} Camels
import Data.Map.Strict (Map)
import {-# SOURCE #-} Dice
import Pieces
import Players

data GameState = GameState {pieceState :: [PieceState], diceState :: DiceState, betState :: BetState, playerState :: [Player], turn :: PlayerId}
instance Show GameState
instance Eq GameState
instance Ord GameState
