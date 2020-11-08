module Pieces where

import Camels
import qualified Data.Vector as Vec
import {-# SOURCE #-} Plates
import Players

data PieceState = CamelStack (Vec.Vector Camel) | Plate {plateKind :: PlateKind, owner :: Player} | Empty
  deriving (Eq, Ord, Show)
