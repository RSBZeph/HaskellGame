-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gstate = return (pictures [viewenemies, viewcharacter])
    where viewcharacter = viewCharacter gstate
          viewenemies = viewEnemies gstate

viewCharacter :: GameState -> Picture
viewCharacter gstate = case shape (character gstate) of
  Model.Rectangle x y -> translate cposx cposy (color green (rectangleWire x y))
    where cposx = xp (character gstate)
          cposy = yp (character gstate)
  Model.Circle x      -> blank

viewEnemies :: GameState -> Picture
viewEnemies gstate = pictures (map toPicture ce)
  where ce = currentenemies gstate

toPicture :: Character -> Picture
toPicture c = case shape c of
  Model.Rectangle x y -> translate (xp c) (yp c) (color blue (rectangleWire x y))
    