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
step secs gstate | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES = do randomNumber <- randomIO -- We show a new random number
                   let newNumber = abs randomNumber `mod` 10
                   return $ GameState (ShowANumber newNumber) 0
                 | otherwise = return $ gstate { elapsedTime = elapsedTime gstate + secs } -- Just update the elapsed time

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate = gstate { infoToShow = ShowAChar c } -- If the user presses a character key, show that one
inputKey _ gstate = gstate -- Otherwise keep the same