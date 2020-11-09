module Camels (Camel (..)) where

newtype Camel = Camel Int
instance Show Camel
instance Eq Camel
instance Ord Camel
