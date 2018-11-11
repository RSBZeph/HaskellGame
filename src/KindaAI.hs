module KindaAi where

import Model
import Graphics.Gloss.Interface.IO.Game


-- makes enemies of type chase move to the position of the player
chaseEnemy :: [Character] -> [Character] -> Character -> [Character]
chaseEnemy chars done p = case chars of
                          [] -> []
                          [a] -> checktype a
                          (a:as) -> recchecktype a as
    where checktype i | cType i == "Chase" = done ++ [b i]
                      | otherwise = done ++ [i]
          recchecktype i j | cType i == "Chase" = chaseEnemy j (done ++ [b i]) p
                           | otherwise = chaseEnemy j (done ++ [i]) p
          b c | x (cpos c) >= x (cpos p) && y (cpos c) <= y (cpos p) = c { cpos = (cpos c){ x = x(cpos c) - cSpeed c, y = y(cpos c) + cSpeed c } }
              | x (cpos c) >= x (cpos p) && y (cpos c) >= y (cpos p) = c { cpos = (cpos c){ x = x(cpos c) - cSpeed c , y = y(cpos c) - cSpeed c } }
              | otherwise = c { cpos = (cpos c){ x = x(cpos c) - cSpeed c } }

-- makes normal enemies move up and down till they reach a specific x position (posx)
normalEnemy :: Float -> [Character] -> [Character] -> [Character]
normalEnemy posx chars done = case chars of
                             [] -> []
                             [a] -> checktype a
                             (a:as) -> recchecktype a as
    where checktype i | cType i == "Normal" = done ++ [b i]
                      | otherwise = done ++ [i]
          recchecktype i j | cType i == "Normal" = normalEnemy posx j (done ++ [b i]) 
                           | otherwise = normalEnemy posx j (done ++ [i]) 
          b c | posx < x (cpos c) = moveUpDown c { cpos = (cpos c){ x = x(cpos c) - cSpeed c } } 
              | otherwise = moveUpDown c

--makes a normal enemy move from the top of the screen to the bottom, repeatedly
moveUpDown :: Character -> Character
moveUpDown c | up c && y (cpos c) >= 260 = c { cpos = (cpos c){ y = 259 }, up = False } 
             | up c && y (cpos c) < 260 = c { cpos = (cpos c){ y = y(cpos c) + cSpeed c } }
             | not(up c) && y (cpos c) <= (-260) = c { cpos = (cpos c){ y = -259 }, up = True }
             | otherwise = c { cpos = (cpos c){ y = y(cpos c) - cSpeed c } }

-- Updates the timer which allows the enemies to shoot            
enemyshoottime :: [Character] -> Float -> [Character]
enemyshoottime [] _ = []
enemyshoottime [a] secs = [a { shootTimer = shootTimer a + secs }]
enemyshoottime (a:as) secs = a { shootTimer = shootTimer a + secs } : enemyshoottime as secs

-- checks if the enemies are allowed to shoot and if they are it will add a new projectile to the list it returns
enemyshoot :: [Character] -> Float -> [Projectile]
enemyshoot [] _ = []
enemyshoot [c] time | shootTimer c >= 1 = [Projectile ((cpos c){x = x (cpos c) - 40}) 30 scaledmg (Model.Rectangle 5 5) 0 EnemyO]
                    | otherwise           = []
    where scaledmg = 3 + 3 / 60 * time
enemyshoot (c:cs) time | shootTimer c >= 1 = Projectile ((cpos c){x = x (cpos c) - 40}) 30 scaledmg (Model.Rectangle 5 5) 0 EnemyO : enemyshoot cs time
                       | otherwise           = enemyshoot cs time
    where scaledmg = 3 + 3 / 60 * time                   

-- resets the timer of when enemies are allowed to shoot                   
resetEnemyTimer :: [Character] -> [Character]
resetEnemyTimer [] = []
resetEnemyTimer [x] | shootTimer x >= 1 = [x {shootTimer = 0}]
                    | otherwise           = [x]
resetEnemyTimer (x:xs) | shootTimer x >= 1 = x {shootTimer = 0} : resetEnemyTimer xs
                       | otherwise           = x : resetEnemyTimer xs