module Pieces (PieceState (..)) where

import {-# SOURCE #-} Camels
import qualified Data.Vector as Vec
import {-# SOURCE #-} Plates
import Players

data PieceState = CamelStack (Vec.Vector Camel) | Plate {plateKind :: PlateKind, owner :: PlayerId} | Empty
instance Show PieceState
instance Eq PieceState
instance Ord PieceState
