{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sort, sortBy)
import Data.Maybe (maybe, catMaybes, fromJust)
import qualified Data.Map.Lazy as Map
import Text.ParserCombinators.ReadP

-- |DateTime stores the timestamp of each guard interaction
data DateTime = DateTime { year :: Int
                         , month :: Int
                         , day :: Int
                         , hour :: Int
                         , minute :: Int
                         } deriving (Show, Eq, Ord) -- Can print, test for equality, and ordering

-- |Guard Number
type GuardNumber = Int

-- |Types of actions a guard performs. Only shift beginning has a guard number,
-- the other two just have a timestamp.
data GuardAction = ShiftBegins DateTime GuardNumber
                 | FallsAsleep DateTime
                 | WakesUp DateTime
                 deriving (Show, Eq) -- Can print and test for equality

-- |getDateTime is a simple helper function to get the DateTime from all GuardAction
getDateTime (ShiftBegins dt _) = dt
getDateTime (FallsAsleep dt) = dt
getDateTime (WakesUp dt) = dt

-- |Add ordering support for GuardAction. Since DateTime already supports
-- ordering (via deriving), it is as simple as extracing the DateTime and
-- comparing.
instance Ord GuardAction where
  compare a b = compare (getDateTime a) (getDateTime b)

-- |Numbers
digit :: ReadP Char
digit = satisfy $ \c -> c >= '0' && c <= '9'

-- |Parser for DateTime
dateTime :: ReadP DateTime
dateTime = do
  char '['
  year <- many1 digit
  char '-'
  month <- many1 digit
  char '-'
  day <- many1 digit
  skipSpaces
  hour <- many1 digit
  char ':'
  minutes <- many1 digit
  char ']'
  return $ DateTime (read year) (read month) (read day) (read hour) (read minutes)

-- |Parser for ShiftBegins
shiftBegins :: ReadP GuardAction
shiftBegins = do
  dt <- dateTime
  string " Guard #"
  num <- many1 digit
  string " begins shift"
  eof
  return $ ShiftBegins dt (read num)

-- |Parser for FallsAsleep
fallsAsleep :: ReadP GuardAction
fallsAsleep = do
  dt <- dateTime
  string " falls asleep"
  eof
  return $ FallsAsleep dt

-- |Parser for WakesUp
wakesUp :: ReadP GuardAction
wakesUp = do
  dt <- dateTime
  string " wakes up"
  eof
  return $ WakesUp dt

-- |guardAction parse all three types of guard actions
guardAction :: ReadP GuardAction
guardAction = choice [shiftBegins, fallsAsleep, wakesUp]

-- |Modifies the parser to return a Maybe GuardAction rather than the
-- [(GuardAction, String)] which is less useful. In this case each line is a
-- single result so we can ignore the "rest" string.
parseMaybe :: ReadP GuardAction -> String -> Maybe GuardAction
parseMaybe p str = do
  let lst = readP_to_S p str
  if length lst > 0
      then Just . fst $ lst !! 0
      else Nothing

-- |Converts actions to map of Start / Stop times
processList :: [GuardAction] -> Map.Map GuardNumber [(Int, Int)]
processList lst = fn lst Map.empty Nothing Nothing
  where
    fn :: [GuardAction] -> Map.Map GuardNumber [(Int, Int)] -> Maybe GuardNumber -> Maybe Int -> Map.Map GuardNumber [(Int, Int)]
    -- End of list, return map
    fn [] m _ _ = m

    -- Shift begins, set Guard Number, but no sleep start time
    fn ((ShiftBegins _ num) : xs) m _ _ = fn xs m (Just num) Nothing

    -- Falls asleep, keep guard number, set sleep minute
    fn ((FallsAsleep dt) : xs) m num _ = fn xs m num (Just $ minute dt)

    -- Wakes up, use sleep minute and guard number from above
    fn ((WakesUp wake) : xs) m (Just guardNum) (Just sleep) = do
      -- Find sleep/wake list from guard or start new
      let lst = maybe [] id (Map.lookup guardNum m)
      let updateSleepAwakeList = (sleep, minute wake) : lst
      let newMap = Map.insert guardNum updateSleepAwakeList m
      fn xs newMap (Just guardNum) Nothing

    -- All other cases shouldn't happen as the order of events should be set, throw an error
    fn (x:xs) _ gn mw = error $ "Should never happen: " ++ show x ++ " " ++ show gn ++ " " ++ show mw

-- |Convert start / stop map to total time map
toTotalTimeMap :: Map.Map GuardNumber [(Int, Int)] -> Map.Map GuardNumber Int
toTotalTimeMap = Map.map (foldl (\tot (st, end) -> tot + (end - st)) 0)

-- |Convert start / stop map to frequency map of minutes
toFrequencyMap :: [(Int, Int)] -> Map.Map Int Int
toFrequencyMap = foldl fn initMap
  where
    initMap = Map.fromList $ zip [0..59] (repeat 0) -- Map: [(0, 0), (1, 0), (2,0)...]
    fn m (start, stop) = foldl fn' m (init [start .. stop]) -- Skip waking minute
    fn' m minute = Map.adjust (+1) minute  m

main :: IO ()
main = do
  putStrLn "Day 4.1: Guard Duty"

  -- Read File and convert to list of actions
  file <- readFile "input.txt"
  let actions = sort $ catMaybes $ map (parseMaybe guardAction) $ lines file
  let processed = processList actions

  -- Gen list of guard's sleep amount
  let timeMap = toTotalTimeMap processed
  let mostSleptGuard = head $ sortBy (\a b -> compare (snd b) (snd a)) $ Map.toList timeMap
  let times = fromJust $ Map.lookup (fst mostSleptGuard) processed
  let freq = toFrequencyMap times

  putStrLn $ "Most Slept Guard: " ++ (show mostSleptGuard)
  putStrLn $ "Ranges: " ++ (show times)
  putStrLn $ "Minutes: " ++ (show freq)

  let mostSleptMinute = head $ sortBy (\(_,freqA) (_,freqB) -> compare freqB freqA) $ Map.toList freq

  putStrLn $ "Most Slept: " ++ show mostSleptMinute

  let solution1 = (fst mostSleptGuard) * (fst mostSleptMinute)

  putStrLn $ "Solution 1: " ++ show solution1

  -- Output:
  -- --------------------------------------
  -- Day 4.1: Guard Duty
  -- Most Slept Guard: (1523,483)
  -- Ranges: [(31,47),(5,43),(40,44),(55,56),(26,52),(14,58),(18,53),(48,51),(4,42),(42,53),(42,57),(57,59),(43,49),(8,22),(47,55),(43,44),(9,51),(1,40),(41,59),(17,36),(17,45),(29,55),(52,57),(14,42),(41,57)]
  -- Minutes: fromList [(0,0),(1,1),(2,1),(3,1),(4,2),(5,3),(6,3),(7,3),(8,4),(9,5),(10,5),(11,5),(12,5),(13,5),(14,7),(15,7),(16,7),(17,9),(18,10),(19,10),(20,10),(21,10),(22,9),(23,9),(24,9),(25,9),(26,10),(27,10),(28,10),(29,11),(30,11),(31,12),(32,12),(33,12),(34,12),(35,12),(36,11),(37,11),(38,11),(39,11),(40,11),(41,13),(42,13),(43,14),(44,12),(45,11),(46,11),(47,11),(48,12),(49,11),(50,11),(51,9),(52,9),(53,7),(54,7),(55,6),(56,5),(57,3),(58,2),(59,0)]
  -- Most Slept: (43,14)
  -- Solution 1: 65489

