module Main where

import Parse
import System.Environment

main :: IO ()
main = do 
      args <- getArgs
      parse args
