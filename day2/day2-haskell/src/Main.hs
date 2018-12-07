{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List

uniq :: Eq a => [a] -> [a]
uniq lst = foldl fn [] lst
  where
    fn xs x = if x `elem` xs
                then xs
                else x : xs

count :: Eq a => a -> [a] -> Int
count x xs = foldl fn 0 xs
  where
    fn s c = if c == x
              then s + 1
              else s


countLine ln = map (flip count ln) (uniq ln)

hasN n lst = if n `elem` lst then 1 else 0





main :: IO ()
main = do
  putStrLn "Day 2: Checksum"
  file <- readFile "input.txt"
  let cnt = map countLine $ lines file
  let numberOf3s = foldl (+) 0 $ map (hasN 3) cnt
  let numberOf2s = foldl (+) 0 $ map (hasN 2) cnt
  putStrLn $ "3: " ++ show numberOf3s
  putStrLn $ "2: " ++ show numberOf2s
  putStrLn $ "Solution: " ++ show (numberOf3s * numberOf2s)
