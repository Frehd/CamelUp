module Bets where

import Camels
import Data.Map.Strict (Map, adjust, elems, filter)
import Data.Set (Set, fromList)
import {-# SOURCE #-} GameState
import Money

newtype BetState = BetState {betState :: Map Camel Bet}
  deriving (Eq, Ord, Show)

data Bet = Bet {camel :: Camel, value :: Money}
  deriving (Eq, Ord, Show)

nextBet :: Bet -> Bet
nextBet (Bet camel' 5) = Bet camel' 3
nextBet (Bet camel' 3) = Bet camel' 2
nextBet (Bet camel' 2) = Bet camel' 1
nextBet (Bet camel' 1) = Bet camel' 0

takeBet :: Bet -> BetState -> BetState
takeBet (Bet camel' _) (BetState betState') = BetState $ Data.Map.Strict.adjust nextBet camel' betState'

possibleBets :: GameState -> Set Bet
possibleBets gameState =
  Data.Set.fromList $
    elems $
      Data.Map.Strict.filter (\bet -> value bet > 0) $
        Bets.betState $ GameState.betState gameState
