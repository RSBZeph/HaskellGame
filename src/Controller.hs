-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game
import Data.List
import KeyInputs
import Collision
import KindaAi


-- | Handle one iteration of the game (update)
step :: Float -> GameState -> IO GameState
step secs gstate | paused gstate || mainmenu gstate || scoremenu gstate || gameover gstate = return gstate --only update the game if you're out of the menus
                 | null (currentenemies gstate) =
                   return $ updategstate { currentenemies = currentenemies updategstate ++ choseWave a, wavenumbers = as } -- add a wave after the last one has been defeated
                 | otherwise                    = return updategstate
    where 
          (a:as) = wavenumbers gstate
          updatetime = gstate { player = (player gstate) { shootTimer = shootTimer (player gstate) + secs }, currentenemies = enemyshoottime (currentenemies gstate) secs, explosions = explosiontime (explosions gstate) secs  }
          updateinput = updateInputDown updatetime { player = (player updatetime) { shootTimer = shootTimer (player updatetime) + secs }, projectiles = moveprojectiles (projectiles gstate) [] }
          updateshootenemy = updateinput {currentenemies = resetEnemyTimer (currentenemies updateinput), projectiles = projectiles updateinput ++ enemyshoot (currentenemies updateinput) }
          updateenemies = characterhit (projectiles updateshootenemy) (currentenemies updateshootenemy)
          updateproj = projectilehit (projectiles updateshootenemy) (currentenemies updateshootenemy)
          updatedead = addDead (updateshootenemy { currentenemies = updateenemies})
          updatechase = chaseEnemy (currentenemies updatedead) [] (player updateshootenemy) 
          updatenormalchar = normalEnemy 100 updatechase []
          updateplayer = playerhit updateshootenemy { projectiles = updateproj }
          updategstate = gstate{ elapsedTime = elapsedTime gstate + secs, currentenemies = updatenormalchar, player = player updateplayer, projectiles = projectiles updateplayer, explosions = explosions updatedead, gameover = health (player gstate) <= 0 } 


--checks if the explosion is done and if not then it adds time to the timer
explosiontime :: [Explosion] -> Float -> [Explosion]
explosiontime [] _ = []
explosiontime [a] secs | timer a > 2 = []
                       | otherwise   = [a{ timer = timer a + secs }]
explosiontime (a:as) secs | timer a > 2 = explosiontime as secs
                          | otherwise   = a{ timer = timer a + secs } : explosiontime as secs            

--if an enemy has <= 0 health, remove it and add it to the explosions list
addDead :: GameState -> GameState
addDead gstate = gstate { currentenemies = newcurrent, explosions = explosions gstate ++ newexplosions, player = (player gstate) { score = newscore } }
    where newcurrent = filter (\x -> health x > 0) (currentenemies gstate)
          newdead = filter (\x -> health x <= 0) (currentenemies gstate)
          newexplosions = map (\x -> Explosion (cpos x) ((width (shape x) + height (shape x)) / 4) 0) newdead
          newscore = score (player gstate) + sum (map score newdead)

--apply a projectiles' speed to its position every frame
moveprojectiles :: [Projectile] -> [Projectile] -> [Projectile]
moveprojectiles p done = case p of
                         []     -> []
                         [a]    -> done ++ b a
                         (a:as) -> moveprojectiles as (done ++ b a)
    where b c | traveled c + speed c < 1500 && typeO c == PlayerO = [c { ppos = (ppos c){ x = x(ppos c) + speed c }, traveled = traveled c + speed c }]
              | traveled c + speed c < 1500 && typeO c == EnemyO  = [c { ppos = (ppos c){ x = x(ppos c) - speed c }, traveled = traveled c + speed c }]
              | otherwise                                         = []