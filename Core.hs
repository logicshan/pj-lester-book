module Main where

import Gm7
import System.Environment

main = do
  [filename] <- getArgs
  contents <- readFile filename
  let result = runProg contents
  putStrLn result
