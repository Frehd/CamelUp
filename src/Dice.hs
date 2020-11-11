module Dice (DiceState (..), Dice, roundEnded, possibleDice, markDiceThrown, resetDiceState) where

import {-# SOURCE #-} Camels
import Data.Map.Strict (Map, foldlWithKey, foldr, fromList, insert)
import Data.Set
import {-# SOURCE #-} qualified GameState

newtype DiceState = DiceState {diceState :: Map Camel Bool}
  deriving (Eq, Ord, Show)

type Dice = Int

resetDiceState :: DiceState
resetDiceState = DiceState (Data.Map.Strict.fromList [(Camel 0, False)])

roundEnded :: GameState.GameState -> Bool --todo maybe place this in gameState + test it
roundEnded gameState =
  Data.Map.Strict.foldr
    (&&)
    True
    (diceState $ GameState.diceState gameState)

possibleDice :: GameState.GameState -> Set Camel
possibleDice
  GameState.GameState
    { GameState.diceState = diceState'
    } =
    Data.Map.Strict.foldlWithKey
      ( \set camel alreadyThrown -> if not alreadyThrown then Data.Set.insert camel set else set
      )
      Data.Set.empty
      $ diceState diceState'

markDiceThrown :: Camel -> DiceState -> DiceState
markDiceThrown camel (DiceState diceState) = DiceState $ Data.Map.Strict.insert camel True diceState
