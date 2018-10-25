-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

--update ja man
-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | elapsedTime gstate > nO_SECS_BETWEEN_CYCLES =
                   return $ gstate { elapsedTime = elapsedTime gstate - nO_SECS_BETWEEN_CYCLES, waves = as, currentenemies = ce }
                 | otherwise                                   =
                   return $ gstate { elapsedTime = elapsedTime gstate + secs } -- Just update the elapsed time
    where wvs@(a:as) = waves gstate
          ce = currentenemies gstate ++ a

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey event@(EventKey (Char c) keystate _ _) gstate = getw event gstate
inputKey _ gstate = gstate -- Otherwise keep the same

getw :: Event -> GameState -> GameState
getw event@(EventKey (Char c) keystate _ _) gstate = geta event newstate
    where newstate | keystate == Down && c == 'w' = gstate { character = (character gstate) { yp = yp (character gstate) + 10 } }
                   | otherwise                    = gstate

geta :: Event -> GameState -> GameState
geta event@(EventKey (Char c) keystate _ _) gstate = gets event newstate
    where newstate | keystate == Down && c == 'a' = gstate { character = (character gstate) { xp = xp (character gstate) - 10 } }
                   | otherwise                    = gstate

gets :: Event -> GameState -> GameState
gets event@(EventKey (Char c) keystate _ _) gstate = getd event newstate
    where newstate | keystate == Down && c == 's' = gstate { character = (character gstate) { yp = yp (character gstate) - 10 } }
                   | otherwise                    = gstate
                   
getd :: Event -> GameState -> GameState
getd (EventKey (Char c) keystate _ _) gstate | keystate == Down && c == 'd' = gstate { character = (character gstate) { xp = xp (character gstate) + 10 } }
                                             | otherwise                    = gstate

--isDown :: EventKey -> Bool
--isDown _ = undefined                                        