-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import KeyInputs


-- | Handle one iteration of the game (update)
step :: Float -> GameState -> IO GameState
step secs gstate | paused gstate || mainmenu gstate || scoremenu gstate || gameover gstate = return gstate --only update the game if you're out of the menus
                 | elapsedTime gstate > wavetime       =
                   return $ updategstate { elapsedTime = elapsedTime updategstate - wavetime, waves = newwvs gstate, currentenemies = newce updategstate } -- add a wave after a certain period of time
                 | otherwise                           =
                   return updategstate
    where 
          updatetime = gstate { player = (player gstate) { shootTimer = shootTimer (player gstate) + secs }, currentenemies = enemyshoottime (currentenemies gstate) secs, explosions = explosiontime (explosions gstate) secs  }
          updateinput = updateInputDown updatetime { player = (player updatetime) { shootTimer = shootTimer (player updatetime) + secs }, projectiles = moveprojectiles (projectiles gstate) [] }
          updateshootenemy = enemyshootgstate updateinput 
          updateenemies = enemyhit (projectiles updateshootenemy) (currentenemies updateshootenemy)
          updateproj = projectilehit (projectiles updateshootenemy) ((player updateshootenemy) : currentenemies updateshootenemy)
          updatedead = addDead (updateshootenemy { currentenemies = updateenemies})
          updatechase = chaseEnemy (currentenemies updatedead) [] (player updateshootenemy) 
          updatenormalchar = normalEnemy (-100) updatechase []
          updategstate = gstate{ elapsedTime = elapsedTime gstate + secs, currentenemies = updatenormalchar, player = player updateshootenemy, projectiles = updateproj, explosions = explosions updatedead, gameover = health (player gstate) <= 0 } 


explosiontime :: [Explosion] -> Float -> [Explosion]
explosiontime [] _ = []
explosiontime [a] secs | timer a > 2 = []
                       | otherwise   = [a{ timer = timer a + secs }]
explosiontime (a:as) secs | timer a > 2 = explosiontime as secs
                          | otherwise   = a{ timer = timer a + secs } : explosiontime as secs

enemyshoottime :: [Character] -> Float -> [Character]
enemyshoottime [] _ = []
enemyshoottime [a] secs = [a { shootTimer = shootTimer a + secs }]
enemyshoottime (a:as) secs = a { shootTimer = shootTimer a + secs } : enemyshoottime as secs

enemyshootgstate :: GameState -> GameState
enemyshootgstate gstate = gstate {currentenemies = resetEnemyTimer (currentenemies gstate), projectiles = projectiles gstate ++ enemyshoot (currentenemies gstate) }

enemyshoot :: [Character] -> [Projectile]
enemyshoot [] = []
enemyshoot [c] | shootTimer c >= 0.5 = [Projectile ((cpos c){x = x (cpos c) - 40}) 50 3 (Model.Rectangle 5 5) 0 EnemyO]
               | otherwise           = []
enemyshoot (c:cs) | shootTimer c >= 0.5 = Projectile ((cpos c){x = x (cpos c) - 40}) 50 3 (Model.Rectangle 5 5) 0 EnemyO : enemyshoot cs
                  | otherwise           = enemyshoot cs


resetEnemyTimer :: [Character] -> [Character]
resetEnemyTimer [] = []
resetEnemyTimer [x] | shootTimer x >= 0.5 = [x {shootTimer = 0 }]
                    | otherwise           = [x]
resetEnemyTimer (x:xs) | shootTimer x >= 0.5 = x {shootTimer = 0} : resetEnemyTimer xs
                       | otherwise           = x : resetEnemyTimer xs

newce :: GameState -> [Character]
newce gstate =  case waves gstate of
                    []     -> currentenemies gstate
                    [a]    -> currentenemies gstate ++ a
                    (a:_)  -> currentenemies gstate ++ a

newwvs :: GameState -> [[Character]]
newwvs gstate = case waves gstate of
                    []     -> []
                    [_]    -> []
                    (a:as) -> as

--check if a character gets hit by a bullet, reduce its lifepoints by the bullet's damage
enemyhit :: [Projectile] -> [Character] -> [Character]
enemyhit [] c = c
enemyhit [x] c = enemyhit' x c
enemyhit (x:xs) c = enemyhit xs (enemyhit' x c)


enemyhit' :: Projectile -> [Character] -> [Character]
enemyhit' p [] = []
enemyhit' p [a] | boxCollision (s p, ppos p) (shape a, cpos a) && ((typeO p == PlayerO && typeOC a == EnemyO) || (typeO p == EnemyO && typeOC a == PlayerO)) = [a{ health = health a - damage p }]
                    | otherwise                                                                                                                                  = [a]
enemyhit' p (a:as) | boxCollision (s p, ppos p) (shape a, cpos a) && ((typeO p == PlayerO && typeOC a == EnemyO) || (typeO p == EnemyO && typeOC a == PlayerO)) = a{ health = health a - damage p } : as
                       | otherwise                                                                                                                                  = a : enemyhit' p as
            

--if an enemy has <= 0 health, remove it and add it to the explosions list
addDead :: GameState -> GameState
addDead gstate = gstate { currentenemies = newcurrent, explosions = explosions gstate ++ newexplosions, player = (player gstate) { score = newscore } }
    where newcurrent = filter (\x -> health x > 0) (currentenemies gstate)
          newdead = filter (\x -> health x <= 0) (currentenemies gstate)
          newexplosions = map (\x -> Explosion (cpos x) ((width (shape x) + height (shape x)) / 4) 0) newdead
          newscore = score (player gstate) + sum (map score newdead)

--check if a bullet hits a character, and remove it from the 'projectiles' list if it does
projectilehit :: [Projectile] -> [Character] -> [Projectile]
projectilehit p c = filterlist p bools
    where bools = map (projectilehit' c) p

--returns true if the projectile hits a character
projectilehit' :: [Character] -> Projectile -> Bool
projectilehit' [] _ = False
projectilehit' [a] p = boxCollision (s p, ppos p) (shape a, cpos a)                                                                                                                                              
projectilehit' (a:as) p | boxCollision (s p, ppos p) (shape a, cpos a) = True
                        | otherwise                                    = projectilehit' as p

filterlist :: [Projectile] -> [Bool] -> [Projectile]
filterlist [] _ = []
filterlist [x] [a] | a         = []
                   | otherwise = [x]
filterlist (x:xs) (a:as) | a         = filterlist xs as
                         | otherwise = x : filterlist xs as


moveprojectiles :: [Projectile] -> [Projectile] -> [Projectile]
moveprojectiles p done = case p of
                         []     -> []
                         [a]    -> done ++ b a
                         (a:as) -> moveprojectiles as (done ++ b a)
    where b c | traveled c + speed c < 1500 && typeO c == PlayerO = [c { ppos = (ppos c){ x = x(ppos c) + speed c }, traveled = traveled c + speed c }]
              | traveled c + speed c < 1500 && typeO c == EnemyO  = [c { ppos = (ppos c){ x = x(ppos c) - speed c }, traveled = traveled c + speed c }]
              | otherwise                                         = []

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