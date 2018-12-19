module Main where

import Data.Char (toUpper, toLower)
import Data.List (sortBy)

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

-- |match checks if the two units would bind
match a b = a /= b && (toUpper a) == (toUpper b)

-- |consumeRun is a single run through the polymer chain
consumeRun [] = []
consumeRun ['\n'] = [] -- Ignore new line
consumeRun [x] = [x]
consumeRun (x:y:rest) =
  if match x y
    then consumeRun rest          -- two units bind, remove them and continue
    else x : consumeRun (y:rest)  -- no bind, shift over one unit and run again

-- |consume runs through chain until no more reductions can be made
consume lst = do
  let run1 = consumeRun lst           -- first run
  let run2 = consumeRun run1          -- second run, based on results of first
  if (length run1) == (length run2)   -- if no change in length, done reducing
    then run1
    else consume run2

-- |removeUnitType removes specific letters from the polymer
removeUnitType ts xs = [ x | x <- xs, not (x `elem` ts) ]

-- |forkThread generates a separate thread to run a process and then returns a
-- handle which can be used to join.
forkThread :: IO () -> IO (MVar ())
forkThread proc = do
  handle <- newEmptyMVar
  _ <- forkFinally proc (\_ -> putMVar handle ())
  return handle

-- |genRunner is the process for removing a single letter and consuming the
-- polymer.
genRunner resLst file letter = do
  let file' = removeUnitType [toLower letter, toUpper letter] file -- Remove 'a' and 'A'
  let run = length $ consume file'  -- run like part 1, on new polymer
  res <- takeMVar resLst
  putMVar resLst $! (letter, run) : res -- tick results to thread safe list
  return ()

main :: IO ()
main = do
  putStrLn "Day 5.1 Polymers"

  -- Read file
  file <- readFile "input.txt"

  let solution1 = consume file
  putStrLn $ "Solution 1: " ++ show (length solution1)

  -- Part 2
  putStrLn "Day 5.2 Better Polymers"

  -- Results list, thread safe
  resultsList <- newMVar []

  -- Partially defining a function with the results list and the normal data
  let runner = genRunner resultsList file

  -- Create a thread for each letter
  threads <- mapM (\letter -> forkThread $ runner letter) ['a'..'z']

  -- Join on all threads
  _ <- mapM_ takeMVar threads

  -- Pull results in main thread
  newRes <- takeMVar resultsList

  -- Sort and find smallest
  let mostEffective = head $ sortBy (\(_, a) (_,b) -> compare a b) newRes

  putStrLn $ "Solution 2: " ++ show mostEffective
