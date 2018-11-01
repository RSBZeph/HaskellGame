-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gstate = return (pictures [viewenemies, viewplayer, viewprojectiles, viewpause, viewdead])
    where viewplayer = viewPlayer gstate
          viewenemies = viewEnemies (currentenemies gstate)
          viewprojectiles = pictures (map ptoPicture (projectiles gstate))
          viewpause | paused gstate = translate (-200) 0 (color red (Text "Paused"))
                    | otherwise     = Blank
          viewdead = viewDead (explosions gstate)

viewPlayer :: GameState -> Picture
viewPlayer gstate = case shape (player gstate) of
  Model.Rectangle a b -> translate cposx cposy (color green (rectangleWire a b))
    where cposx = x (cpos (player gstate))
          cposy = y (cpos (player gstate))

viewEnemies :: [Character] -> Picture
viewEnemies enemies = pictures (map ctoPicture enemies)

viewDead :: [Explosion] -> Picture
viewDead [] = Blank
viewDead [a] | timer a < 1.2 = translate (x (epos a)) (y (epos a)) (color orange (Circle (newrad * timer a * 1.5)))
             | otherwise     = Blank
  where newrad | radius a > 50 = 50
               | otherwise = radius a
viewDead (a:as) | timer a < 1.2 = pictures [translate (x (epos a)) (y (epos a)) (color orange (Circle (newrad * timer a * 1.5))), viewDead as]
                | otherwise     = viewDead as
  where newrad | radius a > 50 = 50
               | otherwise = radius a

ctoPicture :: Character -> Picture
ctoPicture c = case shape c of
  Model.Rectangle a b -> translate (x (cpos c)) (y (cpos c)) (color blue (rectangleWire a b))

ptoPicture :: Projectile -> Picture
ptoPicture p = case s p of
  Model.Rectangle a b -> translate (x (ppos p)) (y (ppos p)) (color red (scale (b/a) 1 (Circle a)))
    