module Main where

import Lib
import System.Environment.Blank (getArgs)
import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs 
  when (null args) (error "An argument should be provided")
  putStrLn . humanSolveRPN . head $ args
