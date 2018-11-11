module KindaAi where

import Model
import Graphics.Gloss.Interface.IO.Game

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

moveUpDown :: Character -> Character
moveUpDown c | up c && y (cpos c) >= 260 = c { cpos = (cpos c){ y = 259 }, up = False } 
             | up c && y (cpos c) < 260 = c { cpos = (cpos c){ y = y(cpos c) + cSpeed c } }
             | not(up c) && y (cpos c) <= (-260) = c { cpos = (cpos c){ y = -259 }, up = True }
             | otherwise = c { cpos = (cpos c){ y = y(cpos c) - cSpeed c } }

enemyshoottime :: [Character] -> Float -> [Character]
enemyshoottime [] _ = []
enemyshoottime [a] secs = [a { shootTimer = shootTimer a + secs }]
enemyshoottime (a:as) secs = a { shootTimer = shootTimer a + secs } : enemyshoottime as secs

enemyshoot :: [Character] -> [Projectile]
enemyshoot [] = []
enemyshoot [c] | shootTimer c >= 1 = [Projectile ((cpos c){x = x (cpos c) - 40}) 30 3 (Model.Rectangle 5 5) 0 EnemyO]
               | otherwise           = []
enemyshoot (c:cs) | shootTimer c >= 1 = Projectile ((cpos c){x = x (cpos c) - 40}) 30 3 (Model.Rectangle 5 5) 0 EnemyO : enemyshoot cs
                  | otherwise           = enemyshoot cs

resetEnemyTimer :: [Character] -> [Character]
resetEnemyTimer [] = []
resetEnemyTimer [x] | shootTimer x >= 1 = [x {shootTimer = 0}]
                    | otherwise           = [x]
resetEnemyTimer (x:xs) | shootTimer x >= 1 = x {shootTimer = 0} : resetEnemyTimer xs
                       | otherwise           = x : resetEnemyTimer xs