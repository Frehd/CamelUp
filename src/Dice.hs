module Dice (DiceState (..), Dice, roundEnded, possibleDice, markDiceThrown) where

import {-# SOURCE #-} Camels
import Data.Map.Strict (Map, foldlWithKey, foldr, insert)
import Data.Set
import {-# SOURCE #-} qualified GameState

newtype DiceState = DiceState {diceState :: Map Camel Bool}
  deriving (Eq, Ord, Show)

type Dice = Int

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
