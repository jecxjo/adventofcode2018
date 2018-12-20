module Main where

import Data.List (uncons)
import qualified Data.Map as Map
import Data.Maybe (maybe, catMaybes)
import Text.ParserCombinators.ReadP

-- |Source of interference
type Source = Int
-- |Distance from interference (using Manhattan Distance)
type Distance = Int
-- |Interference reading
type Reading = (Source, Distance)

-- |X, Y coordinate in space
type Coordinate = (Int, Int)

type Space = Map.Map Coordinate Reading

-- |Parsing Numbers
digit :: ReadP Char
digit = satisfy $ \c -> c >= '0' && c <= '9'

-- |Input Format to Coordinates
coordinate :: ReadP Coordinate
coordinate = do
  x <- many1 digit
  string ", "
  y <- many1 digit
  eof
  return (read x, read y)

-- |Parse lines into Coordinates
parse :: [String] -> [Coordinate]
parse = catMaybes . map (\l -> maybe Nothing (Just . fst . fst) $ uncons $ readP_to_S coordinate l)

-- |Finds the edge of known space using the furthest sources of interference to
-- define where the edge of infinate space begins.
findEdge :: [Coordinate] -> Coordinate
findEdge = foldl (\(maxX, maxY) (x, y) -> (if maxX < x then x else maxX, if maxY < y then y else maxY)) (0,0)

-- |Initial reading, no source and no distance
-- Using distance of -1 since interference sources will have a value of 0
initReading :: Reading
initReading = (-1, -1)

-- |BANG!!! and space is born
initSpace :: Coordinate -> Space
initSpace (maxX, maxY) = Map.fromList [ ( (x,y) , initReading ) | x <- [0 .. maxX], y <- [0 .. maxY] ]

-- |Run through a list of coordinates and insert them into space
-- The Source number is based on their index in the list, all set to distance of 0
createInterference :: Space -> [Coordinate] -> Space
createInterference space coords = foldl fn space indexedCoords
  where
    indexedCoords = zip [0..] coords
    fn sp (src, xy) = Map.insert xy (src, 0) sp

main :: IO ()
main = do
  putStrLn "Day 6.1 Chronal Coordinates"

  file <- readFile "input.txt"

  let coords = parse $ lines file
  putStrLn $ "Coords: " ++ show coords
  putStrLn $ "Len: " ++ show (length coords)

  let edgeOfSpace = findEdge coords
  putStrLn $ "Edge: " ++ show edgeOfSpace
