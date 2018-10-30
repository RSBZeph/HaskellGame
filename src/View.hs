-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gstate = return (pictures [viewenemies, viewplayer, viewprojectiles, viewpause])
    where viewplayer = viewPlayer gstate
          viewenemies = viewEnemies (currentenemies gstate)
          viewprojectiles = viewProjectiles (projectiles gstate)
          viewpause | paused gstate = translate (-200) 0 (color red (Text "Paused"))
                    | otherwise     = Blank

viewPlayer :: GameState -> Picture
viewPlayer gstate = case shape (player gstate) of
  Model.Rectangle a b -> translate cposx cposy (color green (rectangleWire a b))
    where cposx = x (cpos (player gstate))
          cposy = y (cpos (player gstate))
  Model.Circle x      -> blank

viewEnemies :: [Character] -> Picture
viewEnemies enemies = pictures (map ctoPicture enemies)

viewProjectiles :: [Projectile] -> Picture
viewProjectiles list = pictures (map ptoPicture list)

ctoPicture :: Character -> Picture
ctoPicture c = case shape c of
  Model.Rectangle a b -> translate (x (cpos c)) (y (cpos c)) (color blue (rectangleWire a b))

ptoPicture :: Projectile -> Picture
ptoPicture p = case s p of
  Model.Rectangle a b -> translate (x (ppos p)) (y (ppos p)) (color red (rectangleWire a b))
    