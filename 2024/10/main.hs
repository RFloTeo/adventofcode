import System.IO
import Data.Char
import Data.List

type Vector = (Int, Int)

directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]

getDigit :: [String] -> Vector -> Int
getDigit text (x, y)
  | outOfBounds = 999
  | otherwise = digitToInt $ (text !! y) !! x
  where
    outOfBounds = x < 0 || y < 0 || x >= length (head text) || y >= length text

addVector (x1, y1) (x2, y2) = (x1+x2, y1+y2)
subVector (x1, y1) (x2, y2) = (x1-x2, y1-y2)
eqVector (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

getTrailHeads:: [String] -> [Vector]
getTrailHeads ls =
    map snd . concatMap (filter (\(k,v) -> k == '0')) $ keysWithCoords
  where
    keysWithX = map (`zip` [0..]) ls
    addY:: Int -> (Char, Int) -> (Char, Vector)
    addY y (key, x) = (key, (x, y))
    keysWithCoords = zipWith (\list y -> map (addY y) list) keysWithX [0..]

findTrailEnds :: [String] -> Vector -> [Vector]
findTrailEnds map position
  | x == 9 = [position]
  | otherwise = concatMap search directions
  where
    x = getDigit map position
    search d = let newPos = addVector d position in if getDigit map newPos == x + 1 then findTrailEnds map newPos else []

part1:: [String] -> Int
part1 ls = sum . map (length . nub . findTrailEnds ls) . getTrailHeads $ ls

part2:: [String] -> Int
part2 ls = sum . map (length . findTrailEnds ls) . getTrailHeads $ ls

main = do
  input <- readFile "in.txt"
  let ls = lines input
  print . part1 $ ls
  print . part2 $ ls
