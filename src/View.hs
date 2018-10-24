-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewCharacter

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])

viewCharacter :: GameState -> Picture
viewCharacter gstate = case shape (character gstate) of
  Model.Rectangle x y -> translate cposx cposy (color green (rectangleWire x y))
    where cposx = (xp (character gstate))
          cposy = (yp (character gstate))
  Model.Circle x      -> blank
    