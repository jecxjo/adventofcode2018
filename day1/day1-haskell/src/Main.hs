{-# LANGUAGE OverloadedStrings #-}
module Main where

readContents str = map fn $ lines str
  where
    fn :: String -> Int
    fn line = if head line == '+'
                then read $ tail line
                else read line

main :: IO ()
main = do
  putStrLn "Day 1: Sum List of Numbers"
  file <- readFile "input.txt"
  let content = readContents file
  let sum = foldl (+) 0 content
  putStrLn $ "Solution: " ++ show sum
