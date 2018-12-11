{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import Data.Maybe (maybe)
import qualified Data.Map.Lazy as Map
import Text.ParserCombinators.ReadP

type Coordinate = (Int, Int)

-- |Pattern: Number X Y Width Height
data Pattern = Pattern Int Int Int Int Int
  deriving (Show)

-- |Format #123 @ 1,1: 2x3
digit :: ReadP Char
digit = satisfy $ \c -> c >= '0' && c <= '9'

patternNumber :: ReadP Pattern
patternNumber = do
  char '#'
  num <- many1 digit
  skipSpaces
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
  eof
  return $ Pattern (read num) (read x) (read y) (read w) (read h)

-- |Converts a Pattern object into an array of square coordinates it consumes
convertPattern :: Pattern -> [Coordinate]
convertPattern (Pattern _ x y w h) =
  [(x', y') | x' <- [x .. (x + w - 1)], y' <- [y .. (y + h - 1)]]

-- |Converts format string into coordinate squares
stringToSquares str = do
  let pLst = readP_to_S patternNumber str
  if length pLst == 1
    then convertPattern $ fst $ pLst !! 0
    else []

registerCoordinate :: Map.Map Coordinate Int -> Coordinate -> Map.Map Coordinate Int
registerCoordinate st c =
    maybe failCase successCase search
  where
    search = Map.lookup c st
    successCase cnt = Map.insert c (cnt + 1) st
    failCase = Map.insert c 1 st

generateState :: [String] -> Map.Map Coordinate Int
generateState strings = foldl registerCoordinate Map.empty (concat $ map stringToSquares strings)

main :: IO ()
main = do
  putStrLn "Day 3.1: Fabric Patterns"
  file <- readFile "input.txt"
  let state = generateState $ lines file

  -- Find all instances in map with a counter greater than 1
  let f a cnt = if a > 1
                  then cnt + 1
                  else cnt
  let cntOver1 = Map.foldr f 0 state
  putStrLn $ "Solution 1: " ++ show cntOver1
