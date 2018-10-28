-- | This module contains the data types
--   which represent the state of the game
module Model where

--{-# LANGUAGE DuplicateRecordFields #-} --dit zou wat moeten doen???

newtype Name = Name String
--newtype Speed = Speed Float
--newtype Damage = Damage Float --doordat deze aparte datatypes zijn kan je bv niet de speed bij de float van iemands coordinaten optellen
newtype Health = Health Float
data Shape = Rectangle Float Float
           | Circle Float
           
data Character = Character { cx :: Float, cy :: Float, shape :: Shape }
data Projectile = Projectile { px :: Float, py :: Float, damage :: Float, speed :: Float, s :: Shape }
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

wavetime :: Float
wavetime = 3

data GameState = GameState {
                   elapsedTime    :: Float
                 , player         :: Character
                 , waves          :: [[Character]]
                 , currentenemies :: [Character]
                 , pressed        :: [Char]
                 , projectiles    :: [Projectile]
                 }

initialState :: GameState
initialState = GameState 0 (Character 0 0 (Rectangle 40 40)) level1 [] [] []

level1 :: [[Character]]
level1 = [[Character 100 100 (Rectangle 40 40),Character (-100) 100 (Rectangle 60 40)],[Character 150 (-100) (Rectangle 400 40)]]

removefromList :: Eq a => a -> [a] -> [a]
removefromList _ []                 = []
removefromList x (y:ys) | x == y    = removefromList x ys
                    | otherwise = y : removefromList x ys