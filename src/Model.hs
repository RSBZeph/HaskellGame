-- | This module contains the data types
--   which represent the state of the game
module Model where

--our types
data Position = Position {x :: Float, y :: Float}
data Shape = Rectangle { width :: Float, height :: Float }
data TypeObject = EnemyO | PlayerO --this is the type of an object so for example a projectile from an enemy has type EnemyO just like enemy
 deriving (Eq)           
data Character = Character { cpos :: Position, shape :: Shape, health :: Float, cSpeed :: Float, cType :: String, shootTimer :: Float, score :: Int, up :: Bool, typeOC :: TypeObject }
data Projectile = Projectile { ppos :: Position, damage :: Float, speed :: Float, s :: Shape, traveled :: Float, typeO :: TypeObject }
data Explosion = Explosion { epos :: Position, radius :: Float, timer :: Float}

wavetime :: Float
wavetime = 7

data GameState = GameState {
                   elapsedTime    :: Float          --elapsed time since the start of a new game
                 , player         :: Character      --the character controlled by the player
                 , currentenemies :: [Character]    --all enemies currently on screen
                 , explosions     :: [Explosion]    --all explosions of recently deceased enemies
                 , pressed        :: [Char]         --a list of all the buttons that are held down during a single frame
                 , projectiles    :: [Projectile]   --all projectiles on screen
                 , paused         :: Bool           --is the game paused?
                 , mainmenu       :: Bool           --are we in the main menu?
                 , scoremenu      :: Bool           --are we in the score menu?
                 , gameover       :: Bool           --is the player dead?
                 , wavenumbers    :: [Int]          --a infinite list of random Ints which is used as index to chose the next wave from the waves list   
                 }

--this is the first Gamestate
initialState :: [Int] -> GameState
initialState = GameState 0 (Character (Position (-600) 0) (Rectangle 40 40) 200 2 "Player" 0 0 False PlayerO) [] [] [] [] False True False False

--this will return a wave [Character] it receives a random number to choose it
choseWave :: Int -> [Character]
choseWave i = wavesCharacter !! i

--remove something of instance eq from a list
removefromList :: Eq a => a -> [a] -> [a]
removefromList _ []                 = []
removefromList x (y:ys) | x == y    = removefromList x ys
                    | otherwise = y : removefromList x ys

--all our waves we have 10
wavesCharacter :: [[Character]]
wavesCharacter = [[Character (Position 740 (-200)) (Rectangle 20 30) 5 1.3 "Chase" 0 1 False EnemyO,
                  Character (Position 721 100) (Rectangle 30 20) 5 1.3 "Normal" 0 1 True EnemyO,
                  Character (Position 721 (-100)) (Rectangle 30 20) 5 1.3 "Normal" 0 1 False EnemyO,
                  Character (Position 721 200) (Rectangle 20 30) 5 1.3 "Chase" 0 1 True EnemyO,
                  Character (Position 760 0) (Rectangle 50 70) 30 0.8 "Chase" 0 4 False EnemyO],

                  [Character (Position 721 (-100)) (Rectangle 40 60) 20 1.2 "Normal" 0 3 False EnemyO,
                  Character (Position 721 200) (Rectangle 40 60) 20 1.2 "Normal" 0 3 True EnemyO,
                  Character (Position 750 0) (Rectangle 40 60) 20 1.0 "Chase" 0 4 False EnemyO],

                  [Character (Position 721 200) (Rectangle 5 20) 5 1.2 "Normal" 0 1 True EnemyO,
                  Character (Position 721 100) (Rectangle 5 20) 5 1.2 "Normal" 0 1 False EnemyO,
                  Character (Position 721 (-200)) (Rectangle 5 20) 5 1.2 "Normal" 0 1 False EnemyO,
                  Character (Position 721 (-100)) (Rectangle 5 20) 5 1.2 "Normal" 0 1 True EnemyO,
                  Character (Position 721 0) (Rectangle 5 20) 5 1.2 "Chase" 0 1 False EnemyO,
                  Character (Position 750 0) (Rectangle 50 50) 20 0.5 "Chase" 0 6 False EnemyO],

                  [Character (Position 721 (-200)) (Rectangle 40 80) 25 1.3 "Normal" 0 3 False EnemyO,
                  Character (Position 721 0) (Rectangle 40 50) 35 1.1 "Chase" 0 5 False EnemyO,
                  Character (Position 721 200) (Rectangle 50 80) 25 1.3 "Normal" 0 3 True EnemyO],

                  [Character (Position 721 (-100)) (Rectangle 20 30) 10 0.9 "Normal" 0 2 False EnemyO,
                  Character (Position 721 100) (Rectangle 20 30) 10 0.9 "Normal" 0 2 True EnemyO,
                  Character (Position 750 0) (Rectangle 40 50) 30 1.0 "Chase" 0 4 False EnemyO],

                  [Character (Position 721 (-100)) (Rectangle 30 30) 20 0.8 "Normal" 0 3 False EnemyO,
                  Character (Position 721 100) (Rectangle 30 30) 20 1.0 "Normal" 0 3 True EnemyO,
                  Character (Position 750 0) (Rectangle 30 30) 20 1.2 "Chase" 0 3 False EnemyO],

                  [Character (Position 721 200) (Rectangle 40 40) 5 1.2 "Normal" 0 3 True EnemyO,
                  Character (Position 721 0) (Rectangle 40 20) 5 1.4 "Normal" 0 2 False EnemyO,
                  Character (Position 721 (-200)) (Rectangle 40 40) 5 1.2 "Normal" 0 3 False EnemyO,
                  Character (Position 721 0) (Rectangle 40 20) 5 1.4 "Normal" 0 2 True EnemyO],

                  [Character (Position 750 (-200)) (Rectangle 20 50) 10 1.1 "Chase" 0 1 False EnemyO,
                  Character (Position 721 100) (Rectangle 20 50) 10 1.0 "Normal" 0 1 True EnemyO,
                  Character (Position 721 (-100)) (Rectangle 20 50) 10 1.0 "Normal" 0 1 False EnemyO,
                  Character (Position 721 200) (Rectangle 20 50) 10 1.2 "Chase" 0 1 True EnemyO,
                  Character (Position 780 0) (Rectangle 50 80) 20 1.0 "Chase" 0 4 False EnemyO],

                  [Character (Position 721 (-250)) (Rectangle 20 70) 25 1.5 "Chase" 0 3 False EnemyO,
                  Character (Position 721 0) (Rectangle 20 70) 35 1.5 "Chase" 0 5 False EnemyO,
                  Character (Position 721 250) (Rectangle 20 70) 25 1.5 "Chase" 0 3 True EnemyO],

                  [Character (Position 721 200) (Rectangle 10 25) 5 1.0 "Normal" 0 1 True EnemyO,
                  Character (Position 721 100) (Rectangle 10 25) 5 1.0 "Normal" 0 1 False EnemyO,
                  Character (Position 721 (-200)) (Rectangle 10 25) 5 1.0 "Normal" 0 1 False EnemyO,
                  Character (Position 721 (-100)) (Rectangle 10 25) 5 1.0 "Normal" 0 1 True EnemyO,
                  Character (Position 721 0) (Rectangle 30 30) 5 1.0 "Chase" 0 1 False EnemyO,
                  Character (Position 760 0) (Rectangle 50 100) 20 0.7 "Chase" 0 6 False EnemyO]]