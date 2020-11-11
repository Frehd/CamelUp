module Players (Player (..), PlayerId (..), addMoney, getPlayer) where

import {-# SOURCE #-} Bets
import Data.List (findIndex)
import Money
import {-# SOURCE #-} Plates

newtype PlayerId = PlayerId Int
  deriving (Eq, Ord, Show)

data Player = Player {playerId :: PlayerId, money :: Money, plate :: PlayerPlateState, bets :: [Bet]}
  deriving (Eq, Ord, Show)

addMoney :: Int -> Player -> Player
addMoney amount player = player {money = money player + amount}

getPlayer :: PlayerId -> [Player] -> Player
getPlayer playerId' players = case findIndex (\player -> playerId player == playerId') players of
  Just a -> players !! a
  Nothing -> error "Couldn't find the player you were looking for"
