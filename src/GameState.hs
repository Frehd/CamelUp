{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}

module GameState where

import Control.Lens.Combinators (element)
import Control.Lens.Operators
import Data.Map.Strict (Map, adjust, elems, filter, foldr, fromList)
import Data.Set (Set, empty, foldl, fromList, insert, map, singleton, union)
import qualified Data.Vector as Vec
import Relude.List

data GameState = GameState {pieceState :: [PieceState], diceState :: DiceState, betState :: BetState, playerState :: [Player], turn :: Player}
  deriving (Eq, Ord, Show)

newtype DiceState = DiceState {diceState :: Map Camel Bool}
  deriving (Eq, Ord, Show)

newtype BetState = BetState {betState :: Map Camel Bet}
  deriving (Eq, Ord, Show)

data Bet = Bet {camel :: Camel, value :: Money}
  deriving (Eq, Ord, Show)

type Money = Int

data PieceState = CamelStack (Vec.Vector Camel) | Plate {plateKind :: PlateKind, owner :: Player} | Empty
  deriving (Eq, Ord, Show)

data PlayerPlateState = Unused | Placed PlateState
  deriving (Eq, Ord, Show)

data PlateState = PlateState PlateKind Position
  deriving (Eq, Ord, Show)

data PlateKind = Positive | Negative
  deriving (Eq, Ord, Show)

data Player = Player {playerId :: Int, money :: Money, plate :: PlayerPlateState, bets :: [Bet]}
  deriving (Eq, Ord, Show)

newtype Camel = Camel Int
  deriving (Eq, Ord)

newtype Position = Position Int
  deriving (Eq, Ord, Show)

instance Show Camel where
  show (Camel 0) = "green"
  show (Camel 1) = "red"
  show (Camel 2) = "blue"
  show (Camel 3) = "yellow"
  show (Camel 4) = "white"
  show (Camel 5) = "orange"
  show (Camel a) = show a ++ " (unknown camelNum)"

type Dice = Int

data Move = ThrowDice | RoundBet Bet | GameBet Bet | LayPlate PlateState
  deriving (Eq, Ord, Show)

nextBet :: Bet -> Bet
nextBet (Bet camel' 5) = Bet camel' 3
nextBet (Bet camel' 3) = Bet camel' 2
nextBet (Bet camel' 2) = Bet camel' 1
nextBet (Bet camel' 1) = Bet camel' 0

takeBet :: Bet -> BetState -> BetState
takeBet (Bet camel' _) (BetState betState') = BetState $ Data.Map.Strict.adjust nextBet camel' betState'

coordsOfPos :: Position -> Int
coordsOfPos (Position pos) = pos

addPlate :: [PieceState] -> PlateState -> Player -> [PieceState]
addPlate pieceStates (PlateState plateKind' pos) player = (element (coordsOfPos pos) .~ (Plate {plateKind = plateKind', owner = player})) pieceStates

splitCamelStack :: Vec.Vector Camel -> Camel -> (Vec.Vector Camel, Vec.Vector Camel) --(leave behind, camels to move)
splitCamelStack camelStack camel' = (Vec.take camelIndex camelStack, Vec.drop camelIndex camelStack) where
  camelIndex = case Vec.elemIndex camel' camelStack of
    Just i -> i
    Nothing -> -1

execMove :: Move -> GameState -> Set GameState
execMove (LayPlate plateState) gameState = Data.Set.singleton gameState {pieceState = addPlate (pieceState gameState) plateState (turn gameState)}
execMove (RoundBet bet) gameState = Data.Set.singleton (gameState :: GameState) {betState = takeBet bet ((betState :: GameState -> BetState) gameState)}
execMove ThrowDice gameState = Data.Set.fromList [gameState ]
execMove _ _ = Data.Set.empty

possibleBets :: GameState -> Set Bet
possibleBets gameState =
  Data.Set.fromList $
    elems $
      Data.Map.Strict.filter (\bet -> value bet > 0) $
        (betState :: BetState -> Map Camel Bet) $ (betState :: GameState -> BetState) gameState

possiblePlatePositions :: GameState -> Set Position
possiblePlatePositions gameState = recursivePlatePositions (Position 0) gameState Data.Set.empty

recursivePlatePositions :: Position -> GameState -> Set Position -> Set Position
recursivePlatePositions (Position pos) GameState {pieceState = pieces} result | pos == length pieces = result
recursivePlatePositions (Position pos) gameState result =
  if platePlaceable (Position pos) gameState
    then
      Data.Set.union (Data.Set.insert (Position pos) result) $
        recursivePlatePositions (Position (pos + 1)) gameState result
    else
      Data.Set.union result $
        recursivePlatePositions (Position (pos + 1)) gameState result

platePlaceable :: Position -> GameState -> Bool
platePlaceable (Position pos) GameState {pieceState = pieces} | pieces !! pos == Empty =
  case pieces !!? (pos -1) of
    Just (Plate _ _) -> False
    _ ->
      case pieces !!? (pos + 1) of
        Just (Plate _ _) -> False
        _ -> True
platePlaceable _ _ = False

possiblePlates :: GameState -> Set PlateState --todo only allow to place in front of last camel
possiblePlates gameState = case plate $ turn gameState of
  Unused ->
    Data.Set.foldl
      (\set pos -> Data.Set.union (Data.Set.fromList [PlateState Positive pos, PlateState Negative pos]) set)
      Data.Set.empty
      $ possiblePlatePositions gameState
  Placed _ -> Data.Set.empty

roundEnded :: GameState -> Bool
roundEnded gameState =
  Data.Map.Strict.foldr
    (&&)
    False
    ((diceState :: DiceState -> Map Camel Bool) $ (diceState :: GameState -> DiceState) gameState)

possibleMoves :: GameState -> Set Move
possibleMoves gameState =
  Data.Set.insert
    ThrowDice
    ( Data.Set.union
        (Data.Set.map LayPlate $ possiblePlates gameState)
        (Data.Set.map RoundBet $ possibleBets gameState)
    )

standardGameState :: GameState
standardGameState =
  GameState
    { pieceState =
        [ CamelStack $ Vec.fromList [Camel 0],
          Empty,
          Empty,
          Plate {plateKind = Positive, owner = Player {playerId = 0, money = 2, plate = Placed (PlateState Negative (Position 2)), bets = []}}
        ],
      diceState = DiceState (Data.Map.Strict.fromList [(Camel 0, False)]),
      betState = BetState (Data.Map.Strict.fromList [(Camel 0, Bet {camel = Camel 0, value = 1})]),
      playerState = [Player {playerId = 0, money = 2, plate = Unused, bets = []}],
      turn = Player {playerId = 0, money = 2, plate = Unused, bets = []}
    }

initialGameState :: Int -> GameState
initialGameState players =
  GameState
    { pieceState = replicate 10 Empty, --todo find out how many fields
      diceState = DiceState (Data.Map.Strict.fromList [(Camel i, False) | i <- [0 .. 5]]),
      betState = BetState (Data.Map.Strict.fromList [(Camel i, Bet {camel = Camel i, value = 5}) | i <- [0 .. 5]]),
      playerState = [Player {playerId = i, money = 2, plate = Unused, bets = []} | i <- [0 .. players]],
      turn = Player {playerId = 0, money = 2, plate = Unused, bets = []}
    }
