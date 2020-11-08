module Bets where
import Data.Map.Strict (Map)
import Money
import Camels

newtype BetState = BetState {betState :: Map Camel Bet}
instance Show BetState
instance Eq BetState
instance Ord BetState

data Bet = Bet {camel :: Camel, value :: Money}
instance Show Bet
instance Eq Bet
instance Ord Bet