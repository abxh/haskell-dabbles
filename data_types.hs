{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use catMaybes" #-}

import Data.Maybe (Maybe)

data Color
  = Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet

-- data Bool
--   = True
--   | False

data Animal
  = Cat String
  | Dog String
  | Rat
  deriving (Show) -- So you can print stuff in the interpeter.

-- Examples:
-- Cat "A"
-- Dog "B"
-- Rat

data Fish a b
  = Shark a
  | Tuna b
  | CatFish
  deriving (Show) -- You can also construct your own "show" function

data BreedOfFish = HardScaled | SoftScaled deriving (Show)

-- The generic values that are filled contain "data":
-- Shark HardScaled :: Fish BreedOfFish b
-- Tuna 10          :: Tuna a 10
-- CatFish          :: Fish a b
fishify :: Fish a b
fishify = CatFish 

-- Main.{Maybe or Nothing or Just a}
-- data Maybe a = Nothing | Just a deriving (Show)

addJustInt :: Maybe Int -> Maybe Int -> Maybe Int
addJustInt Nothing Nothing = Nothing
addJustInt a Nothing = a
addJustInt Nothing b = b
addJustInt (Just x) (Just y) = Just (x + y)

addJust :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
addJust = zipWith addJustInt

data Metal
  = Gold
  | Silver
  | Bronze
  | Tin
  | Lead
  deriving (Show, Eq)

data Ints
  = BOOL Bool
  | CHAR Char
  | INT Int
  | Integer Integer
  deriving (Eq, Show)

data Tuples a b c d
  = Tuple0 ()
  | Tuple1 a
  | Tuple2 (a, b)
  | Tuple3 (a, b, c)
  | Tuple4 (a, b, c, d)
  deriving (Eq, Show)
