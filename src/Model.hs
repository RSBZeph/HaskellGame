-- | This module contains the data types
--   which represent the state of the game
module Model where

newtype Name = Name String
data Position = Position {x :: Float, y :: Float}
data Shape = Rectangle { width :: Float, height :: Float }
           
data Character = Character { cpos :: Position, shape :: Shape, health :: Float, cSpeed :: Float, cType :: String, shootTimer :: Float }
data Projectile = Projectile { ppos :: Position, damage :: Float, speed :: Float, s :: Shape, traveled :: Float }
data Explosion = Explosion { epos :: Position, radius :: Float, timer :: Float}

wavetime :: Float
wavetime = 2

data GameState = GameState {
                   elapsedTime    :: Float
                 , player         :: Character
                 , waves          :: [[Character]]
                 , currentenemies :: [Character]
                 , explosions     :: [Explosion]
                 , pressed        :: [Char]
                 , projectiles    :: [Projectile]
                 , paused         :: Bool                 
                 }

initialState :: GameState
initialState = GameState 0 (Character (Position (-600) 0) (Rectangle 40 40) 300 2 "Player" 0) level1 [] [] [] [] False

level1 :: [[Character]]
level1 = [[Character (Position 721 (-100)) (Rectangle 40 40) 1 1.8 "Chase" 0,Character (Position 721 200) (Rectangle 40 40) 40 1.8 "Normal" 0],[Character (Position 721 (0)) (Rectangle 40 40) 10 1.8 "Chase" 0]]

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