module Collision where

import Model
import Graphics.Gloss.Interface.IO.Game


--check if two rectangles intersect    
boxCollision :: (Shape, Position) -> (Shape, Position) -> Bool
boxCollision (rec1, pos1) (rec2, pos2) = widthcheck && heightcheck
  where width1 = width rec1
        height1 = height rec1
        width2 = width rec2
        height2 = height rec2
        widthcheck = x pos1 + width1 / 2 >= x pos2 - width2 /2 && x pos1 - width1 / 2 <= x pos2 + width2 / 2
        heightcheck = y pos1 + height1 / 2 >= y pos2 - height2 /2 && y pos1 - height1 / 2 <= y pos2 + height2 / 2

--checks if a player is hit by an enemy projectile        
playerhit :: GameState -> GameState
playerhit gstate = gstate { player = a, projectiles = b }
    where [a] = characterhit (projectiles gstate) [player gstate]
          b   = projectilehit (projectiles gstate) [player gstate]

--check if a character gets hit by a bullet, reduce its lifepoints by the bullet's damage
characterhit :: [Projectile] -> [Character] -> [Character]
characterhit [] c = c
characterhit [x] c = characterhit' x c
characterhit (x:xs) c = characterhit xs (characterhit' x c)


characterhit' :: Projectile -> [Character] -> [Character]
characterhit' p [] = []
characterhit' p [a] | boxCollision (s p, ppos p) (shape a, cpos a) && ((typeO p == PlayerO && typeOC a == EnemyO) || (typeO p == EnemyO && typeOC a == PlayerO)) = [a{ health = health a - damage p }]
                    | otherwise                                                                                                                                  = [a]
characterhit' p (a:as) | boxCollision (s p, ppos p) (shape a, cpos a) && ((typeO p == PlayerO && typeOC a == EnemyO) || (typeO p == EnemyO && typeOC a == PlayerO)) = a{ health = health a - damage p } : as
                       | otherwise = a : characterhit' p as

--check if a bullet hits a character, and remove it from the 'projectiles' list if it does
projectilehit :: [Projectile] -> [Character] -> [Projectile]
projectilehit p c = filterlist p bools
    where bools = map (projectilehit' c) p
    
--returns true if the projectile hits a character
projectilehit' :: [Character] -> Projectile -> Bool
projectilehit' [] _ = False
projectilehit' [a] p = boxCollision (s p, ppos p) (shape a, cpos a)                                                                                                                                              
projectilehit' (a:as) p | boxCollision (s p, ppos p) (shape a, cpos a) = True
                        | otherwise                                    = projectilehit' as p

--helps projectilehit to remove projectiles out a list                       
filterlist :: [Projectile] -> [Bool] -> [Projectile]
filterlist [] _ = []
filterlist [x] [a] | a         = []
                   | otherwise = [x]
filterlist (x:xs) (a:as) | a         = filterlist xs as
                         | otherwise = x : filterlist xs as