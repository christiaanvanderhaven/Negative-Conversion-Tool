module Main where

import Parse
import Model
import View
import Controller
import System.Environment
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do 
      let screen@(sx,sy) = (400,400)
          screenInt = (round sx, round sy)
      args <- getArgs
      initialstate <- parse args
      playIO (InWindow "Negative Conversion Tool" screenInt (0, 0))
        white
        60
        initialstate
        view
        input
        step
