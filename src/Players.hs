module Players where

import {-# SOURCE #-} Bets
import Money
import {-# SOURCE #-} Plates

data Player = Player {playerId :: Int, money :: Money, plate :: PlayerPlateState, bets :: [Bet]}
  deriving (Eq, Ord, Show)
