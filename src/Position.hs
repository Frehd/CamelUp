module Position where

newtype Position = Position Int
  deriving (Eq, Ord, Show)

coordsOfPos :: Position -> Int
coordsOfPos (Position pos) = pos