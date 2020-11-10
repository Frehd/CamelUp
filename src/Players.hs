module Players (Player (..), addMoney) where

import {-# SOURCE #-} Bets
import Money
import {-# SOURCE #-} Plates

data Player = Player {playerId :: Int, money :: Money, plate :: PlayerPlateState, bets :: [Bet]}
  deriving (Eq, Ord, Show)

addMoney :: Int -> Player -> Player
addMoney amount player = player {money = money player + amount}
