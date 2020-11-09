module Bets (BetState (..), Bet (..)) where

import {-# SOURCE #-} Camels
import Data.Map.Strict (Map)
import Money

newtype BetState = BetState {betState :: Map Camel Bet}
instance Show BetState
instance Eq BetState
instance Ord BetState

data Bet = Bet {camel :: Camel, value :: Money}
instance Show Bet
instance Eq Bet
instance Ord Bet
