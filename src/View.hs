-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view ::  [String] -> GameState -> IO Picture
view highscores gstate | mainmenu gstate  = return (pictures [translate (-200) 0 (scale 0.25 0.25 (color red (Text "Press I to start"))), translate (-200) (-200) (scale 0.25 0.25 (color red (Text "Press O to view highscores"))),translate (-650) 200 (color red (Text "Shoot 'em up game"))])
                       | scoremenu gstate = return (pictures [translate (-100) 200 (scale 0.25 0.25 (color red (Text "Press O to return to menu"))), viewhighscores])
                       | gameover gstate  = return (pictures [translate (-200) 0 (scale 0.25 0.25 (color red (Text "Press O to return to menu"))),translate (-500) 200 (color red (Text "Game over")), viewenemies, viewplayer, viewprojectiles, viewpause, viewdead, viewscore, viewhealth])
                       | otherwise        = return (pictures [viewenemies, viewplayer, viewprojectiles, viewpause, viewdead, viewscore, viewhealth])
    where viewplayer = viewPlayer gstate
          viewenemies = pictures (map ctoPicture (currentenemies gstate))
          viewprojectiles = pictures (map ptoPicture (projectiles gstate))
          viewpause | paused gstate = pictures [translate (-200) 0 (color red (Text "Paused")), translate (-200) (-100) (scale 0.25 0.25 (color red (Text "Press Q to return to menu")))]
                    | otherwise     = Blank
          viewdead = viewDead (explosions gstate)
          viewscore = translate (-690) 235 (scale 0.5 0.5 (color red (Text (show (score (player gstate))))))
          viewhealth = translate (-690) (-290) (scale 0.3 0.3 (color red (Text ("HP : " ++ show (health (player gstate))))))
          viewhighscores = pictures (viewHighscores highscores 0)

viewHighscores :: [String] -> Int -> [Picture]
viewHighscores s i | i < length s = translate 0 (150 - 50 * fromIntegral i) (scale 0.25 0.25 (color red (Text (s!!i)))) : viewHighscores s (i + 1)
                   | otherwise    = []

viewPlayer :: GameState -> Picture
viewPlayer gstate = case shape (player gstate) of
  Model.Rectangle a b -> translate cposx cposy (color green (rectangleWire a b))
    where cposx = x (cpos (player gstate))
          cposy = y (cpos (player gstate))

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