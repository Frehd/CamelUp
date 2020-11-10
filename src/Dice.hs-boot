module Dice (DiceState (..), Dice) where

import {-# SOURCE #-} Camels
import Data.Map.Strict (Map)

newtype DiceState = DiceState {diceState :: Map Camel Bool}
instance Show DiceState
instance Eq DiceState
instance Ord DiceState

type Dice = Int
