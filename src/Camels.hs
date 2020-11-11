{-# LANGUAGE LambdaCase #-}

module Camels (Camel (..), moveCamel, lastCamel) where

import Control.Lens.Combinators (element)
import Control.Lens.Operators
import Data.List
import qualified Data.Vector as Vec
import {-# SOURCE #-} GameState
import Pieces (PieceState (..))
import {-# SOURCE #-} Plates
import Players

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

lastCamel :: [PieceState] -> Int
lastCamel pieceStates = case findIndex
  ( \case
      CamelStack _ -> True
      _ -> False
  )
  pieceStates of
  Just a -> a
  Nothing -> error "Couldn't find the camel you were looking for"

splitCamelStack :: Vec.Vector Camel -> Camel -> (Vec.Vector Camel, Vec.Vector Camel) --(leave behind, camels to move)
splitCamelStack camelStack camel = (Vec.take camelIndex camelStack, Vec.drop camelIndex camelStack)
  where
    camelIndex = case Vec.elemIndex camel camelStack of
      Just i -> i
      Nothing -> error "Couldn't find the camel you were looking for"

containsCamel :: PieceState -> Camel -> Bool
containsCamel (CamelStack camelVec) camel = Vec.elem camel camelVec
containsCamel _ _ = False

findCamel :: Camel -> [PieceState] -> Int
findCamel camel pieceStates = case Data.List.findIndex (`containsCamel` camel) pieceStates of
  Just a -> a
  Nothing -> error "Couldn't find the camel you were looking for"

removeCamels :: Vec.Vector Camel -> [PieceState] -> [PieceState]
removeCamels removalCamels pieceStates = case pieceStates !! camelPos of
  CamelStack camelVec -> (element camelPos .~ CamelStack standingCamels) pieceStates
    where
      (standingCamels, _) = splitCamelStack camelVec $ Vec.head removalCamels
  where
    camelPos = findCamel (Vec.head removalCamels) pieceStates

addCamels :: Vec.Vector Camel -> Int -> GameState -> GameState
addCamels camels pos gameState = case GameState.pieceState gameState !! pos of
  CamelStack camelVec -> gameState {pieceState = insertCamels (camelVec Vec.++ camels)}
  Empty -> gameState {pieceState = insertCamels camels}
  Plate Positive plateOwner -> addCamels camels (pos + 1) gameState {playerState = payPlate plateOwner}
  Plate Negative plateOwner -> addCamels camels (pos -1) gameState {playerState = payPlate plateOwner}
  where
    insertCamels = \newCamels -> (element pos .~ CamelStack newCamels) (GameState.pieceState gameState)
    payPlate = \plateOwnerId -> (element (playerIndex plateOwnerId) .~ addMoney 1 (plateOwner plateOwnerId)) (GameState.playerState gameState)
      where
        plateOwner = \plateOwnerId -> GameState.playerState gameState !! playerIndex plateOwnerId
        playerIndex = \plateOwnerId -> case findIndex (\player -> playerId player == plateOwnerId) (GameState.playerState gameState) of
          Just i -> i
          Nothing -> error "Couldn't find the camel you were looking for"

moveCamel :: Camel -> Int -> GameState -> GameState
moveCamel camel spaces gameState = case GameState.pieceState gameState !! camelPos of
  CamelStack camelVec ->
    addCamels movingCamels (camelPos + spaces) $
      gameState {pieceState = removeCamels movingCamels (GameState.pieceState gameState)}
    where
      (_, movingCamels) = splitCamelStack camelVec camel
  where
    camelPos = findCamel camel (GameState.pieceState gameState)
