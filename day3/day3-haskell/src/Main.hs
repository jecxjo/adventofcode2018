{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (maybe)
import qualified Data.Map.Lazy as Map
import Text.ParserCombinators.ReadP

-- |Coordinate data type, (X,Y)
type Coordinate = (Int, Int)

-- |Pattern Id, the #1 part of the string
type Id = Int

-- |Pattern: ID X Y Width Height
data Pattern = Pattern { patternId :: Id, x :: Int, y :: Int, w :: Int, h :: Int }
  deriving (Show)

-- |Format #123 @ 1,1: 2x3
digit :: ReadP Char
digit = satisfy $ \c -> c >= '0' && c <= '9'

patternNumber :: ReadP Pattern
patternNumber = do
  char '#'            -- Ignore, has no value for use to resolve
  pId <- many1 digit  -- Save as pId, contains 1 or more (many1) number characters (digit)
  skipSpaces          -- Ignore whitespace
  char '@'
  skipSpaces
  x <- many1 digit
  char ','
  y <- many1 digit
  char ':'
  skipSpaces
  w <- many1 digit
  char 'x'
  h <- many1 digit
  eof                 -- Always match for end of line to know we pulled all of height

  -- Return a Pattern object, read knows the types to convert strings to per the definition on line 16
  -- pId is an ID, x is an Int, etc.
  return $ Pattern (read pId) (read x) (read y) (read w) (read h)

-- |Converts a Pattern object into an array of square coordinates it consumes
-- Make a list using 'list comprehension':
--   * Of tuple x' and y'
--   * Such that x' is the value x to x plus width minus 1
--   * Such that y' is the value y to y plus height minus 1
-- x' and y' are independent of each other so they operate in their own loops,
--   * x = 1, y = 1
--   * x = 1, y = 2
--   * x = 2, y = 1
--   * x = 2, y = 2
convertPattern :: Pattern -> [Coordinate]
convertPattern (Pattern _ x y w h) =
  [(x', y') | x' <- [x .. (x + w - 1)], y' <- [y .. (y + h - 1)]]

-- |Converts input string into coordinate squares
stringToSquares :: String -> (Id, [Coordinate])
stringToSquares str = do
  let pLst = readP_to_S patternNumber str -- do parsing using patternNumber, returns list of sucesses
  if length pLst == 1
    then do
      let p = fst $ pLst !! 0             -- Index 0, first part of tuple [(Pattern, "rest of string")]
      (patternId p, convertPattern p)     -- Return (Id, [Coordinate])
    else ((-1), [])

-- |Register a (X,Y) coordinate into a map, key being the coordinate and the value being a list of Ids of
-- patterns that use this unit square.
registerCoordinate :: Map.Map Coordinate [Id] -> (Id, Coordinate) -> Map.Map Coordinate [Id]
registerCoordinate st (i, c) = maybe failCase successCase search
  where
    search = Map.lookup c st                      -- Lookup key "X,Y"
    successCase ids = Map.insert c (i : ids) st   -- If found, append this ID to list of IDs
    failCase = Map.insert c [i] st                -- Else create new list of IDs, this being the only ID as its a new unit square

-- |generateState links all the above functions together converting an array of
-- input strings and outputing a map of IDs w/ keys being their coordinates
generateState :: [String] -> Map.Map Coordinate [Id]
generateState strings = do
  let squares = map stringToSquares strings                                 -- Output: [(Id, [Coordinate])]
  let squares' = concat $ map (\(i, cx) -> map (\c -> (i, c)) cx) squares   -- Output: [(Id, Coordinate)]
  foldl registerCoordinate Map.empty squares'

-- PART 2

-- |convertState converts a map of Coordinates to [ID], to a new map of (ID, Bool)
-- The Bool is True if multiple IDs consume this Coordinate, and False if only one.
convertState :: Map.Map Coordinate [Id] -> Map.Map Id Bool
convertState st = Map.foldl f Map.empty st
  where
    -- No IDs at this coordinate, move along
    f st' [] = st'

    -- Since a single ID exists for this coordinate, check to see if the ID has
    -- already been found in a 2+ consumed unit square.  If this Pattern
    -- overlaps, we don't want to later report it doesn't overlap if the last
    -- unit square it consumes is a single case.
    f st' [l] = maybe (Map.insert l False st') (\oldVal -> Map.insert l (oldVal || False) st') (Map.lookup l st')

    -- Multiple IDs at this coordinate, force all IDs to be True as they overlap at least once
    f st' lst = foldl (\s l -> Map.insert l True s) st' lst

-- |findFalse looks through a map of ID / Bool and finds a False case.
findFalse :: Map.Map Id Bool -> Maybe Id
findFalse = Map.foldlWithKey f Nothing
  where
    -- f Result Key Value
    f _ k False = Just k
    f m _ True = m

main :: IO ()
main = do
  putStrLn "Day 3.1: Fabric Patterns"

  -- Read File and convert to map of Coordinate, [ID]
  file <- readFile "input.txt"
  let state = generateState $ lines file

  -- Find all instances in map with a counter greater than 1
  let f a cnt = if (length a) > 1
                  then cnt + 1
                  else cnt
  let cntOver1 = Map.foldr f 0 state
  putStrLn $ "Solution 1: " ++ show cntOver1

  -- Find instance where there are no overlaps
  let reverseState = convertState state
  putStrLn $ "Solution 2: " ++ (show $ findFalse reverseState)
