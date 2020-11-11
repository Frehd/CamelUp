module Pieces (PieceState (..), removePlates) where

import {-# SOURCE #-} Camels
import qualified Data.Vector as Vec
import {-# SOURCE #-} Plates
import Players

data PieceState = CamelStack (Vec.Vector Camel) | Plate {plateKind :: PlateKind, owner :: PlayerId} | Empty
  deriving (Eq, Ord, Show)

removePlates :: [PieceState] -> [PieceState]
removePlates = map removePlate
  where
    removePlate =
      \piece -> case piece of
        Plate _ _ -> Empty
        _ -> piece
