module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
       content <- readFile "highscores.txt"
       let highscores = lines content
       playIO (InWindow "Shoot'em up game" (1400, 600) (0, 0)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              initialState     -- Initial state
              (view highscores)-- View function
              input            -- Event function
              step             -- Step function