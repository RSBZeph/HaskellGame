-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


-- | Handle one iteration of the game (update)
step :: Float -> GameState -> IO GameState
step secs gstate | elapsedTime gstate > wavetime =
                   return $ updategstate { elapsedTime = elapsedTime gstate - wavetime, waves = newwvs gstate, currentenemies = newce updategstate } -- add a wave after a certain period of time
                 | otherwise                     =
                   return $ updategstate { elapsedTime = elapsedTime gstate + secs }
    where updateinput = updateInputDown gstate { projectiles = moveprojectiles (projectiles gstate) [] }
          updatechar = characterhit (projectiles updateinput) (currentenemies updateinput)
          updateproj = projectilehit (projectiles updateinput) (currentenemies updateinput)
          updatechase = chaseEnemy (updatechar) [] (player gstate)
          updategstate = gstate{ currentenemies = updatechase, player = player updateinput, projectiles = updateproj }      


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

--check if a character gets hit by a bullet, reduce its lifepoints by the bullet's damage and remove the character is health <= 0
characterhit :: [Projectile] -> [Character] -> [Character]
characterhit [] c = c
characterhit [x] c = characterhit' x c
characterhit (x:xs) c = characterhit xs newc
    where newc = characterhit' x c

characterhit' :: Projectile -> [Character] -> [Character]
characterhit' p [] = []
characterhit' p [a] | boxCollision (s p, ppos p) (shape a, cpos a) = damagestep
                    | otherwise                                    = [a]
    where damagestep | health a - damage p <= 0 = []
                     | otherwise = [a{ health = health a - damage p }]
characterhit' p (a:as) | boxCollision (s p, ppos p) (shape a, cpos a) = damagestep
                       | otherwise                                    = a : characterhit' p as
    where damagestep | health a - damage p <= 0 = as
                     | otherwise = a{ health = health a - damage p } : as


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
filterlist [x] [a] | a || traveled x > 200 = []
                   | otherwise             = [x]
filterlist (x:xs) (a:as) | a || traveled x > 200 = filterlist xs as
                         | otherwise             = x : filterlist xs as


--if a button is pressed, it gets added to the 'pressed' list in gamestate
--if the button is no longer pressed, it gets removed from the list
--this method checks which buttons are held down so that their effects (moving, shooting, etc.) may be repeated over several frames
--instead of only applying on the frame in which the button initially was pressed
updateInputDown :: GameState -> GameState
updateInputDown gstate | 'w' `elem` pressed gstate = updateInputDown gstate { player = (player gstate) { cpos = (cpos (player gstate)){ y = py + 2 } }, pressed = removefromList 'w' (pressed gstate) }
                       | 'a' `elem` pressed gstate = updateInputDown gstate { player = (player gstate) { cpos = (cpos (player gstate)){ x = px - 2 } }, pressed = removefromList 'a' (pressed gstate) }
                       | 's' `elem` pressed gstate = updateInputDown gstate { player = (player gstate) { cpos = (cpos (player gstate)){ y = py - 2 } }, pressed = removefromList 's' (pressed gstate) }
                       | 'd' `elem` pressed gstate = updateInputDown gstate { player = (player gstate) { cpos = (cpos (player gstate)){ x = px + 2 } }, pressed = removefromList 'd' (pressed gstate) }
                       | 'j' `elem` pressed gstate = updateInputDown gstate --hoe zorg je ervoor dat de speler maar om de x seconden kan schieten?
                       { projectiles = Projectile ((cpos (player gstate)){x = 20 + x (cpos (player gstate))}) 2 3 (Model.Rectangle 5 5) 0 : projectiles gstate, pressed = removefromList 'j' (pressed gstate) }
                       | otherwise                 = gstate
    where px = x (cpos (player gstate))
          py = y (cpos (player gstate))

moveprojectiles :: [Projectile] -> [Projectile] -> [Projectile]
moveprojectiles [] _ = []
moveprojectiles [a] done = done ++ [b]
    where b =  a { ppos = (ppos a){ x = x(ppos a) + speed a }, traveled = traveled a + speed a }
moveprojectiles (a:as) done = moveprojectiles as (done ++ [b])
    where b =  a { ppos = (ppos a){ x = x(ppos a) + speed a }, traveled = traveled a + speed a }

chaseEnemy :: [Character] -> [Character] -> Character -> [Character]
chaseEnemy [] _ _= []
chaseEnemy [a] done p | cType a == "Chase" = done ++ [b]
                      | otherwise = done ++ [a]
    where b | x (cpos a) >= x (cpos p) && y (cpos a) <= y (cpos p) = a { cpos = (cpos a){ x = x(cpos a) - cSpeed a } { y = y(cpos a) + cSpeed a }}
            | x (cpos a) >= x (cpos p) && y (cpos a) >= y (cpos p) = a { cpos = (cpos a){ x = x(cpos a) - cSpeed a } { y = y(cpos a) - cSpeed a }}
            | otherwise = a { cpos = (cpos a){ x = x(cpos a) - cSpeed a }}
chaseEnemy (a:as) done p | cType a == "Chase" = chaseEnemy as (done ++ [b]) p
                         | otherwise = chaseEnemy as (done ++ [a]) p
    where b | x (cpos a) >= x (cpos p) && y (cpos a) <= y (cpos p) = a { cpos = (cpos a){ x = x(cpos a) - cSpeed a } { y = y(cpos a) + cSpeed a }}
            | x (cpos a) >= x (cpos p) && y (cpos a) >= y (cpos p) = a { cpos = (cpos a){ x = x(cpos a) - cSpeed a } { y = y(cpos a) - cSpeed a }}
            | otherwise = a { cpos = (cpos a){ x = x(cpos a) - cSpeed a }}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey event@(EventKey (Char c) keystate _ _) gstate = getw event gstate
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
getj (EventKey (Char c) keystate _ _) gstate = newstate
    where newstate | c == 'j' && keystate == Up   = gstate { pressed = removefromList 'j' (pressed gstate) }
                   | c == 'j'                     = gstate { pressed = 'j' : pressed gstate }
                   | otherwise                    = gstate                     