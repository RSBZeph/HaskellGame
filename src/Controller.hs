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
                   return $ gstate { elapsedTime = elapsedTime gstate - wavetime, waves = newwvs, currentenemies = ce, character = character upchar }
                 | otherwise                                   =
                   return $ gstate { elapsedTime = elapsedTime gstate + secs, character = character upchar } -- Just update the elapsed time
    where wvs = waves gstate
          ce = case wvs of
            []     -> currentenemies gstate
            [a]    -> currentenemies gstate ++ a
            (a:as) -> currentenemies gstate ++ a
          newwvs = case wvs of
            []     -> []
            [a]    -> [a]
            (a:as) -> as
          upchar = gstate { character = character (updateCharacter gstate) }


updateCharacter :: GameState -> GameState
updateCharacter gstate | 'w' `elem` pressed gstate = updateCharacter gstate { character = (character gstate) { yp = yp (character gstate) + 10 }, pressed = removefromList 'w' (pressed gstate) }
                       | 'a' `elem` pressed gstate = updateCharacter gstate { character = (character gstate) { xp = xp (character gstate) - 10 }, pressed = removefromList 'a' (pressed gstate) }
                       | 's' `elem` pressed gstate = updateCharacter gstate { character = (character gstate) { yp = yp (character gstate) - 10 }, pressed = removefromList 's' (pressed gstate) }
                       | 'd' `elem` pressed gstate = updateCharacter gstate { character = (character gstate) { xp = xp (character gstate) + 10 }, pressed = removefromList 'd' (pressed gstate) }
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
getd (EventKey (Char c) keystate _ _) gstate = newstate
    where newstate | c == 'd' && keystate == Up   = gstate { pressed = removefromList 'd' (pressed gstate) }
                   | c == 'd'                     = gstate { pressed = 'd' : pressed gstate }
                   | otherwise                    = gstate         