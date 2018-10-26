-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss.Data.Point

newtype Name = Name String
newtype Speed = Speed Int
newtype Health = Health Float
newtype Position = Position (Float, Float)
newtype Damage = Damage Float
data Shape = Rectangle Float Float
           | Circle Float
           
data Character = Character {xp :: Float, yp :: Float, shape :: Shape}
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

wavetime :: Float
wavetime = 3

data GameState = GameState {
                   elapsedTime    :: Float
                 , character      :: Character
                 , waves          :: [[Character]]
                 , currentenemies :: [Character]
                 , pressed        :: [Char]
                 }

initialState :: GameState
initialState = GameState 0 (Character 0 0 (Rectangle 40 40)) level1 [] []

level1 :: [[Character]]
level1 = [[Character 100 100 (Rectangle 40 40),Character (-100) 100 (Rectangle 60 40)],[Character 150 (-100) (Rectangle 400 40)]]

removefromList :: Eq a => a -> [a] -> [a]
removefromList _ []                 = []
removefromList x (y:ys) | x == y    = removefromList x ys
                    | otherwise = y : removefromList x ys