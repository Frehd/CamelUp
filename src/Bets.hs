module Bets (BetState (..), Bet (..), possibleBets, takeBet, resetBetState, payBets) where

import Camels
import Data.Map.Strict (Map, adjust, elems, filter, fromList)
import Data.Set (Set, fromList)
import {-# SOURCE #-} GameState
import Money
import Players

newtype BetState = BetState {betState :: Map Camel Bet}
  deriving (Eq, Ord, Show)

data Bet = Bet {camel :: Camel, value :: Money}
  deriving (Eq, Ord, Show)

nextBet :: Bet -> Bet
nextBet (Bet camel' 5) = Bet camel' 3
nextBet (Bet camel' 3) = Bet camel' 2
nextBet (Bet camel' 2) = Bet camel' 1
nextBet (Bet camel' 1) = Bet camel' 0
nextBet bet = error $ "invalid bet: " ++ show bet

takeBet :: Bet -> BetState -> BetState
takeBet (Bet camel' _) (BetState betState') = BetState $ Data.Map.Strict.adjust nextBet camel' betState'

resetBetState :: BetState
resetBetState = BetState (Data.Map.Strict.fromList [(Camel i, Bet {camel = Camel i, value = 5}) | i <- [0 .. 5]])

possibleBets :: GameState -> Set Bet
possibleBets gameState =
  Data.Set.fromList $
    elems $
      Data.Map.Strict.filter (\bet -> value bet > 0) $
        Bets.betState $ GameState.betState gameState

evaluateBet :: GameState -> Bet -> Money
evaluateBet gameState (Bet betCamel betValue)
  | firstCamel (pieceState gameState) == betCamel = betValue
  | secondCamel (pieceState gameState) == betCamel = 1
  | otherwise = 0

payBet :: GameState -> Player -> Player
payBet gameState player = foldl (\resultPlayer bet -> resultPlayer {money = evaluateBet gameState bet + money resultPlayer}) player $ bets player

payBets :: GameState -> [Player]
payBets gameState = map (payBet gameState) (playerState gameState)
