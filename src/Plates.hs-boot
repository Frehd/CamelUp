module Plates (PlayerPlateState (..), PlateState (..), PlateKind (..)) where

import Position

data PlayerPlateState = Unused | Placed PlateState
instance Show PlayerPlateState
instance Eq PlayerPlateState
instance Ord PlayerPlateState

data PlateState = PlateState PlateKind Position
instance Show PlateState
instance Eq PlateState
instance Ord PlateState

data PlateKind = Positive | Negative
instance Show PlateKind
instance Eq PlateKind
instance Ord PlateKind
