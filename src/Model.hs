-- | This module contains the data types
--   which represent the state of the game
module Model where

newtype Name = Name String
data Position = Position {x :: Float, y :: Float}
data Shape = Rectangle { width :: Float, height :: Float }
           | Circle {radius :: Float }
           
data Character = Character { cpos :: Position, shape :: Shape, health :: Float }
data Projectile = Projectile { ppos :: Position, damage :: Float, speed :: Float, s :: Shape, traveled :: Float }
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

wavetime :: Float
wavetime = 2

data GameState = GameState {
                   elapsedTime    :: Float
                 , player         :: Character
                 , waves          :: [[Character]]
                 , currentenemies :: [Character]
                 , pressed        :: [Char]
                 , projectiles    :: [Projectile]
                 }

initialState :: GameState
initialState = GameState 0 (Character (Position 0 0) (Rectangle 40 40) 300) level1 [] [] []

level1 :: [[Character]]
level1 = [[Character (Position 100 100) (Rectangle 40 40) 1,Character (Position(-100) 100) (Rectangle 60 40) 40],[Character (Position 150 (-100)) (Rectangle 400 40) 100]]

removefromList :: Eq a => a -> [a] -> [a]
removefromList _ []                 = []
removefromList x (y:ys) | x == y    = removefromList x ys
                    | otherwise = y : removefromList x ys

boxCollision :: (Shape, Position) -> (Shape, Position) -> Bool
boxCollision (rec1, pos1) (rec2, pos2) = widthcheck && heightcheck
  where width1 = width rec1
        height1 = height rec1
        width2 = width rec2
        height2 = height rec2
        widthcheck = x pos1 + width1 / 2 >= x pos2 - width2 /2 && x pos1 - width1 / 2 <= x pos2 + width2 / 2
        heightcheck = y pos1 + height1 / 2 >= y pos2 - height2 /2 && y pos1 - height1 / 2 <= y pos2 + height2 / 2