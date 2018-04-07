import Control.Monad
import Data.List
import System.IO

maxTemp = 5526

getInput :: IO [Int]
getInput = do
  junk <- getLine
  n <- getLine
  let s = words n
  return $ map (read::String->Int) s
  
closestZero :: Int -> Int -> Int
closestZero z x
  |(z^2) < (x^2) = z
  |(z^2) > (x^2) = x
  |otherwise = max x z
  
main :: IO()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE
  inputs <- getInput
  let answer = (if inputs == [] then 0 else foldr closestZero maxTemp inputs)
  putStr $ show answer
  
  
  

