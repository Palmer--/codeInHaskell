import Control.Monad
import Data.List
import System.IO

maxPi = 10000000

readLines :: IO [Int]
readLines = do
  nLines <- getLine
  inputs <- replicateM (read nLines) (fmap read getLine :: IO Int)
  return inputs
        
findClosest' :: (Integral a) => [a] -> a -> a
findClosest' [] a = a
findClosest' (x:y:s) a = findClosest' (y:s) (min a (y-x))
findClosest' (x:[]) a = a
 
findClosest :: [Int] -> Int
findClosest [] = maxPi
findClosest a = findClosest' (sort a) maxPi

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    input <- readLines
    let res = findClosest input
    putStrLn (show res)
        
