-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gstate = return (pictures [viewenemies, viewplayer, viewprojectiles])
    where viewplayer = viewPlayer gstate
          viewenemies = viewEnemies (currentenemies gstate)
          viewprojectiles = viewProjectiles (projectiles gstate)

viewPlayer :: GameState -> Picture
viewPlayer gstate = case shape (player gstate) of
  Model.Rectangle x y -> translate cposx cposy (color green (rectangleWire x y))
    where cposx = cx (player gstate)
          cposy = cy (player gstate)
  Model.Circle x      -> blank

viewEnemies :: [Character] -> Picture
viewEnemies enemies = pictures (map ctoPicture enemies)

viewProjectiles :: [Projectile] -> Picture
viewProjectiles list = pictures (map ptoPicture list)

ctoPicture :: Character -> Picture
ctoPicture c = case shape c of
  Model.Rectangle x y -> translate (cx c) (cy c) (color blue (rectangleWire x y))

ptoPicture :: Projectile -> Picture
ptoPicture p = case s p of
  Model.Rectangle x y -> translate (px p) (py p) (color red (rectangleWire x y))
    