module KeyInputs where

import Model
import Graphics.Gloss.Interface.IO.Game

-- | Handle user input

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
                       { player = (player gstate) { shootTimer = 0 }, projectiles = Projectile ((cpos (player gstate)){x = 35 + x (cpos (player gstate))}) 2 3 (Model.Rectangle 5 5) 0 PlayerO : projectiles gstate, pressed = removefromList 'j' pg }
                       
                       | otherwise                 = gstate
    where px = x (cpos (player gstate))
          py = y (cpos (player gstate))
          pg = pressed gstate

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey event@(EventKey (Char c) keystate _ _) gstate | mainmenu gstate     = geti event gstate --if you're in the main menu, only check i and o
                                                       | scoremenu gstate    = geto event gstate
                                                       | gameover gstate   = geto event gstate 
                                                       | paused gstate       = getp event gstate
                                                       | otherwise           = getw event gstate --if the game is paused, only check if the player is unpausing or not
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
    where newstate | c == 'o' && keystate == Down = check
                   | otherwise                    = gstate    
          check | gameover gstate = gstate { mainmenu = True, gameover = False }
                | otherwise         = gstate { scoremenu = not (scoremenu gstate), mainmenu = not (mainmenu gstate) } 