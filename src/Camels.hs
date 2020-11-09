module Camels (Camel (..), moveCamel) where

import Control.Lens.Combinators (element)
import Control.Lens.Operators
import Data.List
import qualified Data.Vector as Vec
import Pieces (PieceState (..))

newtype Camel = Camel Int
  deriving (Eq, Ord)

instance Show Camel where
  show (Camel 0) = "green"
  show (Camel 1) = "red"
  show (Camel 2) = "blue"
  show (Camel 3) = "yellow"
  show (Camel 4) = "white"
  show (Camel 5) = "orange"
  show (Camel a) = show a ++ " (unknown camelNum)"

splitCamelStack :: Vec.Vector Camel -> Camel -> (Vec.Vector Camel, Vec.Vector Camel) --(leave behind, camels to move)
splitCamelStack camelStack camel = (Vec.take camelIndex camelStack, Vec.drop camelIndex camelStack)
  where
    camelIndex = case Vec.elemIndex camel camelStack of
      Just i -> i
      Nothing -> -1

containsCamel :: PieceState -> Camel -> Bool
containsCamel (CamelStack camelVec) camel = Vec.elem camel camelVec
containsCamel _ _ = False

findCamel :: Camel -> [PieceState] -> Int
findCamel camel pieceStates = case Data.List.findIndex (`containsCamel` camel) pieceStates of
  Just a -> a
  Nothing -> -1

removeCamels :: Vec.Vector Camel -> [PieceState] -> [PieceState]
removeCamels removalCamels pieceStates = case pieceStates !! camelPos of
  CamelStack camelVec -> (element camelPos .~ CamelStack standingCamels) pieceStates
    where
      (standingCamels, _) = splitCamelStack camelVec $ Vec.head removalCamels
  where
    camelPos = findCamel (Vec.head removalCamels) pieceStates

addCamels :: Vec.Vector Camel -> Int -> [PieceState] -> [PieceState]
addCamels camels pos pieceStates = (element pos .~ CamelStack a) pieceStates
  where
    a = case pieceStates !! pos of
      CamelStack camelVec -> camelVec Vec.++ camels
      Empty -> camels

moveCamel :: Camel -> Int -> [PieceState] -> [PieceState]
moveCamel camel spaces pieceStates = case pieceStates !! camelPos of
  CamelStack camelVec ->
    addCamels movingCamels (camelPos + spaces) $
      removeCamels movingCamels pieceStates
    where
      (_, movingCamels) = splitCamelStack camelVec camel
  where
    camelPos = findCamel camel pieceStates

moveCamel' :: Camel -> Int -> [PieceState] -> [PieceState]
moveCamel' camel spaces pieceStates = case pieceStates !! camelPos of --todo test if the same as moveCamel
  CamelStack camelVec -> (element camelPos .~ CamelStack standingCamels) pieceStates
    where
      (standingCamels, movingCamels) = splitCamelStack camelVec camel --splitCamelStack camelVec camel
  where
    camelPos = findCamel camel pieceStates
