import System.IO
import Data.List

type Vector = (Int, Int)
data Direction = Up | Dn | Lt | Rt deriving (Eq, Ord, Show)
type Segment = (Direction, Int, Int)

addVector (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

up = (0, -1)
rt = (1, 0)
dn = (0, 1)
lt = (-1, 0)

nextPos:: Vector -> Vector -> Vector
nextPos (x, y) (lx, ly)
  | x < lx = (x + 1, y)
  | y < ly = (0, y + 1)
  | otherwise = (-1, -1)

charFromMap:: [String] -> Vector -> Char
charFromMap mp (x, y)
  | outOfBounds = '.'
  | otherwise = (mp !! y) !! x
  where
    outOfBounds = x < 0 || y < 0 || x >= length (head mp) || y >= length mp

findRegion:: [String] -> Char -> [Vector] -> Vector -> (Int, Int, [Vector])
findRegion mp prevChar prevPoss pos
  | currChar /= prevChar = (0, 1, prevPoss)
  | pos `elem` prevPoss = (0, 0, prevPoss)
  | otherwise = (1 + area, perimeter, v4)
  where
    currChar = charFromMap mp pos
    newPrevs = pos : prevPoss
    (a1, p1, v1) = findRegion mp currChar newPrevs (addVector pos up)
    (a2, p2, v2) = findRegion mp currChar v1 (addVector pos rt)
    (a3, p3, v3) = findRegion mp currChar v2 (addVector pos dn)
    (a4, p4, v4) = findRegion mp currChar v3 (addVector pos lt)
    area = a1 + a2 + a3 + a4
    perimeter = p1 + p2 + p3 + p4

traverseMap:: [String] -> Vector -> [Vector] -> Int
traverseMap mp pos history
  | pos == (-1, -1) = 0
  | pos `elem` history = traverseMap mp next history 
  | otherwise = (area * perimeter) + traverseMap mp next newHist
  where
    bounds = ((length . head $ mp) - 1, length mp - 1)
    next = nextPos pos bounds
    currChar = charFromMap mp pos
    (area, perimeter, newHist) = findRegion mp currChar history pos 

part1:: [String] -> Int
part1 s = traverseMap s (0, 0) []

getSegment:: Direction -> Vector -> Segment
getSegment Up (x, y) = (Up, y + 1, x)
getSegment Dn (x, y) = (Dn, y - 1, x)
getSegment Rt (x, y) = (Rt, x + 1, y)
getSegment Lt (x, y) = (Lt, x - 1, y)

findRegion':: [String] -> Direction -> Char  -> [Vector] -> Vector -> (Int, [Segment], [Vector])
findRegion' mp dir prevChar prevPoss pos
  | currChar /= prevChar = (0, newSegment, prevPoss)
  | pos `elem` prevPoss = (0, [], prevPoss)
  | otherwise = (1 + area, perimeter, v4)
  where
    currChar = charFromMap mp pos
    newSegment = [getSegment dir pos]
    newPrevs = pos : prevPoss
    (a1, p1, v1) = findRegion' mp Up currChar newPrevs (addVector pos up)
    (a2, p2, v2) = findRegion' mp Rt currChar v1 (addVector pos rt)
    (a3, p3, v3) = findRegion' mp Dn currChar v2 (addVector pos dn)
    (a4, p4, v4) = findRegion' mp Lt currChar v3 (addVector pos lt)
    area = a1 + a2 + a3 + a4
    perimeter = p1 ++ p2 ++ p3 ++ p4

segmentsToSides:: [Segment] -> [Segment]
segmentsToSides [] = []
segmentsToSides ((dir, main, sec):ss)
  | (dir, main, sec + 1) `elem` ss = segmentsToSides ss
  | otherwise = (dir, main, sec) : segmentsToSides ss

traverseMap':: [String] -> Vector -> [Vector] -> Int
traverseMap' mp pos history
  | pos == (-1, -1) = 0
  | pos `elem` history = traverseMap' mp next history 
  | otherwise = (area * perimeter) + traverseMap' mp next newHist
  where
    bounds = ((length . head $ mp) - 1, length mp - 1)
    next = nextPos pos bounds
    currChar = charFromMap mp pos
    (area, segments, newHist) = findRegion' mp Up currChar history pos 
    perimeter = length . segmentsToSides . sort $ segments

part2:: [String] -> Int
part2 s = traverseMap' s (0,0) []

main = do
  input <- readFile "in.txt"
  let ls = lines input
  print . part1 $ ls
  print . part2 $ ls
