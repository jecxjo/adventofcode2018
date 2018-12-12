{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List

-- |uniq takes a list, adding a single instance or each element to the result.
uniq :: Eq a => [a] -> [a]
uniq lst = foldl fn [] lst
  where
    fn xs x = if x `elem` xs
                then xs
                else x : xs

-- |Given a value (arg1) and a list of values (arg2), return the count of found
-- in list
count :: Eq a => a -> [a] -> Int
count x xs = foldl fn 0 xs
  where
    fn s c = if c == x
              then s + 1
              else s

-- |countLine counts the number of instances of each letter in a string
-- List of letters is made uniq, to remove dups when doing the search. No point
-- in having "aa" search for "a" twice.
countLine ln = map (flip count ln) (uniq ln)

-- |if list has value, return 1, else return 0
hasN n lst = if n `elem` lst then 1 else 0

-- Part 2

-- |Filters list, selecting only items that have an element that repeats twice
-- or three times.
onlyTwosAndThrees :: [String] -> [String]
onlyTwosAndThrees = filter f -- Point Notation, not displaying argument 1 here because its assumed
  where
    f str = do
      let cnt = countLine str
      (2 `elem` cnt) || (3 `elem` cnt)

-- |Find two strings from a list, where their indexed characters are off by one
-- digit each.
findOneDiff [] = Nothing
findOneDiff [x] = Nothing
findOneDiff (x:xs) = isOneOff x xs
  where
    -- Nothing exhasted list xs with string x, start over with the next value for x
    isOneOff _ [] = findOneDiff xs

    -- Using a and b, zip each character together and count the number of
    -- differences. zip a b -> [(a[0], b[0]), (a[1], b[1])...]
    isOneOff a (b:bs) = if countDiff (zip a b) == 1
                            then Just $ intersect a b -- Success, get list of where the two strings match
                            else isOneOff a bs

    -- Sum up the number of indexes of the two lists that don't match.
    countDiff = foldl (\s (n, m) -> if (n == m) then s else s + 1) 0

main :: IO ()
main = do
  putStrLn "Day 2: Checksum"
  -- Read File
  file <- readFile "input.txt"

  -- Create list of lists of letter counts
  let cnt = map countLine $ lines file

  -- Add up the number of instances where there are 3 letters matching
  let numberOf3s = foldl (+) 0 $ map (hasN 3) cnt
  -- Add up the number of instances where there are 2 letters matchingk
  let numberOf2s = foldl (+) 0 $ map (hasN 2) cnt
  -- Results are 2 count times 3 count
  putStrLn $ "Solution: " ++ show (numberOf3s * numberOf2s)

  -- Part 2
  let oneOff = findOneDiff . onlyTwosAndThrees $ lines file
  putStrLn $ "Solution 2: " ++ show (maybe "No solution" id oneOff)
