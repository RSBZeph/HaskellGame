module Main where

import Controller
import Model
import View
import KeyInputs
import System.Random

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
       content <- readFile "highscores.txt"
       let highscores = lines content
       gen <- getStdGen
       let rngnrs = randomRs (0, 6 :: Int) gen
       playIO (InWindow "Shoot 'em up game" (1400, 600) (0, 0)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              initialState     -- Initial state
              (view highscores)-- View function
              input            -- Event function
              (step rngnrs)    -- Step function, alongside a list of random numbers used to determine the next incoming wave