-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss.Data.Point

data Name = Name String
data Speed = Speed Int
data Health = Health Float
data Position = Position Float Float
data Damage = Damage Float
data Shape = Rectangle Float Float
           | Circle Float
           
data Character = Character {xp :: Float, yp :: Float, shape :: Shape}
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , character   :: Character
                 }

initialState :: GameState
initialState = GameState ShowNothing 0 (Character 0 100 (Rectangle 200 20))