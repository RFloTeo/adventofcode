import System.IO
import Data.List

type Vector = (Int, Int)

addVector (x1,y1) (x2, y2) = (x1+x2, y1+y2)

getMove '^' = (0, 1)
getMove '>' = (1, 0)
getMove 'v' = (0, -1)
getMove '<' = (-1, 0)
getMove c = (0, 0)

takeEveryOther:: [a] -> [a]
takeEveryOther [] = []
takeEveryOther [c] = [c]
takeEveryOther (c:c':cs) = c : takeEveryOther cs

doStep:: [Vector] -> Char -> [Vector]
doStep hist c = addVector (getMove c) (head hist) : hist

getVisits:: String -> [Vector]
getVisits  = foldl doStep [(0,0)]

part1:: String -> Int
part1 = length . nub . getVisits

part2:: String -> Int
part2 s = length . nub $ santa ++ roboSanta
  where
    santa = getVisits . takeEveryOther $ s
    roboSanta = getVisits . takeEveryOther . tail $ s

main = do
  input <- readFile "in.txt"
  print . part1 $ input
  print . part2 $ input
