-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


-- | Handle one iteration of the game (update)
step :: Float -> GameState -> IO GameState
step secs gstate | elapsedTime gstate > nO_SECS_BETWEEN_CYCLES =
                   return $ gstate { elapsedTime = elapsedTime gstate - nO_SECS_BETWEEN_CYCLES, waves = as, currentenemies = ce, character = (character upchar) }
                 | otherwise                                   =
                   return $ gstate { elapsedTime = elapsedTime gstate + secs, character = (character upchar) } -- Just update the elapsed time
    where wvs@(a:as) = waves gstate
          ce = currentenemies gstate ++ a
          upchar = gstate { character = character (updateCharacter gstate) }


updateCharacter :: GameState -> GameState
updateCharacter gstate | elem 'w' (pressed gstate) = updateCharacter gstate { character = (character gstate) { yp = yp (character gstate) + 10 }, pressed = removefromList 'w' (pressed gstate) }
                       | elem 'a' (pressed gstate) = updateCharacter gstate { character = (character gstate) { xp = xp (character gstate) - 10 }, pressed = removefromList 'a' (pressed gstate) }
                       | elem 's' (pressed gstate) = updateCharacter gstate { character = (character gstate) { yp = yp (character gstate) - 10 }, pressed = removefromList 's' (pressed gstate) }
                       | elem 'd' (pressed gstate) = updateCharacter gstate { character = (character gstate) { xp = xp (character gstate) + 10 }, pressed = removefromList 'd' (pressed gstate) }
                       | otherwise                 = gstate


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey event@(EventKey (Char c) keystate _ _) gstate = getw event gstate
inputKey _ gstate = gstate -- Otherwise keep the same

getw :: Event -> GameState -> GameState
getw event@(EventKey (Char c) keystate _ _) gstate = geta event newstate
    where newstate | c == 'w' && keystate == Up   = gstate { pressed = removefromList 'w' (pressed gstate) }
                   | c == 'w'                     = gstate { pressed = 'w' : (pressed gstate) }
                   | otherwise                    = gstate

geta :: Event -> GameState -> GameState
geta event@(EventKey (Char c) keystate _ _) gstate = gets event newstate
    where newstate | c == 'a' && keystate == Up   = gstate { pressed = removefromList 'a' (pressed gstate) }
                   | c == 'a'                     = gstate { pressed = 'a' : (pressed gstate) }
                   | otherwise                    = gstate

gets :: Event -> GameState -> GameState
gets event@(EventKey (Char c) keystate _ _) gstate = getd event newstate
    where newstate | c == 's' && keystate == Up   = gstate { pressed = removefromList 's' (pressed gstate) }
                   | c == 's'                     = gstate { pressed = 's' : (pressed gstate) }
                   | otherwise                    = gstate
                   
getd :: Event -> GameState -> GameState
getd (EventKey (Char c) keystate _ _) gstate = newstate
    where newstate | c == 'd' && keystate == Up   = gstate { pressed = removefromList 'd' (pressed gstate) }
                   | c == 'd'                     = gstate { pressed = 'd' : (pressed gstate) }
                   | otherwise                    = gstate

--isDown :: EventKey -> Bool
--isDown _ = undefined          

removefromList :: Eq a => a -> [a] -> [a]
removefromList _ []                 = []
removefromList x (y:ys) | x == y    = removefromList x ys
                    | otherwise = y : removefromList x ys