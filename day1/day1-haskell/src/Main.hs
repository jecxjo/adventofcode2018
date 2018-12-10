{-# LANGUAGE OverloadedStrings #-}
module Main where

readContents str = map fn $ lines str
  where
    fn :: String -> Int
    fn line = if head line == '+'
                then read $ tail line
                else read line

findFirstDup :: [Int] -> Maybe Int
findFirstDup lst = fn [] lst
  where
    fn _ [] = Nothing
    fn [] [x] = Nothing
    fn ys (x:xs) = if x `elem` ys
                      then Just x
                      else fn (x:ys) xs

-- | step2 freq deltas
step2 _ [] = Nothing
step2 [] (d:ds) = step2 [d] ds
step2 (f:fs) (d:ds) = do
  let sum = f + d
  if sum `elem` (f:fs)
      then Just sum
      else step2 (sum : f : fs) ds

main :: IO ()
main = do
  putStrLn "Day 1.1: Sum List of Numbers"
  file <- readFile "input.txt"
  let content = readContents file
  let sum = foldl (+) 0 content
  putStrLn $ "Solution 1: " ++ show sum

  let step2Res = step2 [] (cycle content)
  putStrLn $ "Solution 2: " ++ (maybe "No answer" show step2Res)
