module Main where

import Examples.Fibonacci
import Examples.ShortestPath
-- import Examples.Knapsack
import Examples.FinalValIter
import Examples.SP



main = do 
  -- run shortest path
  let a = runSthg 
  -- run fibonacci
  let b = runFib 3
  putStrLn $ show a 
  putStrLn $ show b
  --putStrLn $ show runknap 
  return ()


