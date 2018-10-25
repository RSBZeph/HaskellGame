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
step secs gstate | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES = -- We show a new random number
                   do randomNumber <- randomIO
                      let newNumber = abs randomNumber `mod` 10
                      return $ GameState (ShowANumber newNumber) 0 (Character 10 20 (Model.Rectangle 20 20)) --deze moet nog anders
                 | otherwise = return $ gstate { elapsedTime = elapsedTime gstate + secs } -- Just update the elapsed time

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate | c == 'w' = gstate { character = (character gstate) { xp = 100 } } -- If the user presses a character key, show that one
                                          | otherwise = gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same