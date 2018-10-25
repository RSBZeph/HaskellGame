-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss.Data.Point

data Name = Name String
data Speed = Speed Int
data Health = Health Float
data Position = Position (Float, Float)
data Damage = Damage Float
data Shape = Rectangle Float Float
           | Circle Float
           
data Character = Character {xp :: Float, yp :: Float, shape :: Shape}
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 3

data GameState = GameState {
                   elapsedTime    :: Float
                 , character      :: Character
                 , waves          :: [[Character]]
                 , currentenemies :: [Character]
                 }

initialState :: GameState
initialState = GameState 0 (Character 0 0 (Rectangle 40 40)) level1 []

level1 :: [[Character]]
level1 = [[(Character 100 100 (Rectangle 40 40)),(Character (-100) 100 (Rectangle 60 40))],[(Character 150 (-100) (Rectangle 400 40))]]