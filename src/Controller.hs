-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List


-- | Handle one iteration of the game (update)
step :: Float -> GameState -> IO GameState
step secs gstate | paused gstate || mainmenu gstate || scoremenu gstate = return gstate --only update the game if you're out of the menus
                 | elapsedTime gstate > wavetime       =
                   return $ updategstate { elapsedTime = elapsedTime updategstate - wavetime, waves = newwvs gstate, currentenemies = newce updategstate } -- add a wave after a certain period of time
                 | otherwise                           =
                   return updategstate
    where 
          updatetime = gstate { player = (player gstate) { shootTimer = shootTimer (player gstate) + secs }, currentenemies = enemyshoottime (currentenemies gstate) secs, explosions = explosiontime (explosions gstate) secs  }
          updateinput = updateInputDown updatetime { player = (player updatetime) { shootTimer = shootTimer (player updatetime) + secs }, projectiles = moveprojectiles (projectiles gstate) [] }
          updatechar = characterhit (projectiles updateinput) (currentenemies updateinput)
          updateproj = projectilehit (projectiles updateinput) (currentenemies updateinput)
          updatedead = addDead (updateinput { currentenemies = updatechar})
          updatechase = chaseEnemy (currentenemies updatedead) [] (player updateinput) 
          updategstate = gstate{ elapsedTime = elapsedTime gstate + secs, currentenemies = updatechase, player = (player updateinput) { score = score (player updatedead) }, projectiles = updateproj, explosions = explosions updatedead } 
             

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
characterhit :: [Projectile] -> [Character] -> [Character]
characterhit [] c = c
characterhit [x] c = characterhit' x c
characterhit (x:xs) c = characterhit xs newc
    where newc = characterhit' x c

characterhit' :: Projectile -> [Character] -> [Character]
characterhit' p [] = []
characterhit' p [a] | boxCollision (s p, ppos p) (shape a, cpos a) = [a{ health = health a - damage p }]
                    | otherwise                                    = [a]
characterhit' p (a:as) | boxCollision (s p, ppos p) (shape a, cpos a) = a{ health = health a - damage p } : as
                       | otherwise                                    = a : characterhit' p as


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
    where b c | traveled c + speed c < 1500 = [c { ppos = (ppos c){ x = x(ppos c) + speed c }, traveled = traveled c + speed c }]
              | otherwise                   = []

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


--if a button is pressed, it gets added to the 'pressed' list in gamestate
--if the button is no longer pressed, it gets removed from the list
--this method checks which buttons are held down so that their effects (moving, shooting, etc.) may be repeated over several frames
--instead of only applying on the frame in which the button initially was pressed
updateInputDown :: GameState -> GameState
updateInputDown gstate | 'w' `elem` pg = updateInputDown gstate { player = (player gstate) { cpos = (cpos (player gstate)){ y = py + 2 } }, pressed = removefromList 'w' pg }
                       | 'a' `elem` pg = updateInputDown gstate { player = (player gstate) { cpos = (cpos (player gstate)){ x = px - 2 } }, pressed = removefromList 'a' pg }
                       | 's' `elem` pg = updateInputDown gstate { player = (player gstate) { cpos = (cpos (player gstate)){ y = py - 2 } }, pressed = removefromList 's' pg }
                       | 'd' `elem` pg = updateInputDown gstate { player = (player gstate) { cpos = (cpos (player gstate)){ x = px + 2 } }, pressed = removefromList 'd' pg }
                       | 'j' `elem` pg && shootTimer (player gstate) >= 0.3 = gstate 
                       { player = (player gstate) { shootTimer = 0 }, projectiles = Projectile ((cpos (player gstate)){x = 20 + x (cpos (player gstate))}) 2 3 (Model.Rectangle 5 5) 0 : projectiles gstate, pressed = removefromList 'j' pg }
                       
                       | otherwise                 = gstate
    where px = x (cpos (player gstate))
          py = y (cpos (player gstate))
          pg = pressed gstate


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey event@(EventKey (Char c) keystate _ _) gstate | mainmenu gstate     = geti event gstate --if you're in the main menu, only check i and o
                                                       | scoremenu gstate    = geto event gstate 
                                                       | not (paused gstate) = getw event gstate
                                                       | otherwise           = getp event gstate --if the game is paused, only check if the player is unpausing or not
inputKey _ gstate = gstate -- Otherwise keep the same

getw :: Event -> GameState -> GameState
getw event@(EventKey (Char c) keystate _ _) gstate = geta event newstate
    where newstate | c == 'w' && keystate == Up   = gstate { pressed = removefromList 'w' (pressed gstate) }
                   | c == 'w'                     = gstate { pressed = 'w' : pressed gstate }
                   | otherwise                    = gstate

geta :: Event -> GameState -> GameState
geta event@(EventKey (Char c) keystate _ _) gstate = gets event newstate
    where newstate | c == 'a' && keystate == Up   = gstate { pressed = removefromList 'a' (pressed gstate) }
                   | c == 'a'                     = gstate { pressed = 'a' : pressed gstate }
                   | otherwise                    = gstate

gets :: Event -> GameState -> GameState
gets event@(EventKey (Char c) keystate _ _) gstate = getd event newstate
    where newstate | c == 's' && keystate == Up   = gstate { pressed = removefromList 's' (pressed gstate) }
                   | c == 's'                     = gstate { pressed = 's' : pressed gstate }
                   | otherwise                    = gstate
                   
getd :: Event -> GameState -> GameState
getd event@(EventKey (Char c) keystate _ _) gstate = getj event newstate
    where newstate | c == 'd' && keystate == Up   = gstate { pressed = removefromList 'd' (pressed gstate) }
                   | c == 'd'                     = gstate { pressed = 'd' : pressed gstate }
                   | otherwise                    = gstate  
                   
getj :: Event -> GameState -> GameState
getj event@(EventKey (Char c) keystate _ _) gstate = getp event newstate
    where newstate | c == 'j' && keystate == Up   = gstate { pressed = removefromList 'j' (pressed gstate) }
                   | c == 'j'                     = gstate { pressed = 'j' : pressed gstate }
                   | otherwise                    = gstate                     

getp :: Event -> GameState -> GameState
getp event@(EventKey (Char c) keystate _ _) gstate = getq event newstate
    where newstate | c == 'p' && keystate == Down = gstate { paused = not (paused gstate) }
                   | otherwise                    = gstate   
                   
getq :: Event -> GameState -> GameState
getq (EventKey (Char c) keystate _ _) gstate = newstate
    where newstate | paused gstate && c == 'q' && keystate == Down = gstate { mainmenu = True, paused = False }
                   | otherwise                                     = gstate
    

--this input is only usefull in the menus                   
geti :: Event -> GameState -> GameState
geti event@(EventKey (Char c) keystate _ _) gstate = geto event newstate
    where newstate | mainmenu gstate && c == 'i' && keystate == Down = initialState { mainmenu = False }
                   | otherwise                                       = gstate  

geto :: Event -> GameState -> GameState
geto event@(EventKey (Char c) keystate _ _) gstate = newstate
    where newstate | c == 'o' && keystate == Down = gstate { scoremenu = not (scoremenu gstate), mainmenu = not (mainmenu gstate) }
                   | otherwise                    = gstate     