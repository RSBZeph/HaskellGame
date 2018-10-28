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
                   return $ gstate { elapsedTime = elapsedTime gstate - wavetime, waves = newwvs, currentenemies = ce, player = newplayer, projectiles = newproj }
                 | otherwise                                   =
                   return $ gstate { elapsedTime = elapsedTime gstate + secs, player = newplayer, projectiles = newproj } -- Just update the elapsed time
    where wvs = waves gstate
          ce = case wvs of
            []     -> currentenemies gstate
            [a]    -> currentenemies gstate ++ a
            (a:as) -> currentenemies gstate ++ a
          newwvs = case wvs of --kunnen deze twee cases niet samen op de een of andere manier?
            []     -> []
            [a]    -> [a]
            (a:as) -> as
          p = gstate { projectiles = updateProjectiles (projectiles gstate) []}
          updatestate = updateInputDown p
          newplayer = player updatestate
          newproj = projectiles updatestate

--if a button is pressed, it gets added to the 'pressed' list in gamestate
--if the button is no longer pressed, it gets removed from the list
--this method checks which buttons are held down so that their effects (moving, shooting, etc.) may be repeated over several frames
--instead of only applying on the frame in which the button was pressed
updateInputDown :: GameState -> GameState
updateInputDown gstate | 'w' `elem` pressed gstate = updateInputDown gstate { player = (player gstate) { cy = pcy + 2 }, pressed = removefromList 'w' (pressed gstate) }
                       | 'a' `elem` pressed gstate = updateInputDown gstate { player = (player gstate) { cx = pcx - 2 }, pressed = removefromList 'a' (pressed gstate) }
                       | 's' `elem` pressed gstate = updateInputDown gstate { player = (player gstate) { cy = pcy - 2 }, pressed = removefromList 's' (pressed gstate) }
                       | 'd' `elem` pressed gstate = updateInputDown gstate { player = (player gstate) { cx = pcx + 2 }, pressed = removefromList 'd' (pressed gstate) }
                       | 'j' `elem` pressed gstate = updateInputDown gstate --hoe zorg je ervoor dat de speler maar om de x seconden kan schieten?
                       { projectiles = Projectile pcx pcy 5 3 (Model.Rectangle 5 5) : projectiles gstate, pressed = removefromList 'j' (pressed gstate) }
                       | otherwise                 = gstate
    where pcx = cx (player gstate)
          pcy = cy (player gstate)


updateProjectiles :: [Projectile] -> [Projectile] -> [Projectile]
updateProjectiles [] _ = []
updateProjectiles [a] done = done ++ [b]
    where b =  a { px = px a + speed a }
updateProjectiles (a:as) done = updateProjectiles as (done ++ [b])
    where b =  a { px = px a + speed a }


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