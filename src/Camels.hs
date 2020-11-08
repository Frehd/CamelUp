module Camels where
import qualified Data.Vector as Vec

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
splitCamelStack camelStack camel' = (Vec.take camelIndex camelStack, Vec.drop camelIndex camelStack) where
  camelIndex = case Vec.elemIndex camel' camelStack of
    Just i -> i
    Nothing -> -1