import System.IO

countLevels:: String -> Int
countLevels [] = 0
countLevels ('(' : ps) = 1 + countLevels ps
countLevels (')' : ps) = countLevels ps - 1
countLevels (c:cs) = countLevels cs 

part1:: String -> Int
part1 = countLevels

findBasement:: Int -> String -> Int
findBasement x (c:cs)
  | x < 0 = 0
  | c == '(' = 1 + findBasement (x+1) cs
  | c == ')' = 1 + findBasement (x-1) cs
  | otherwise = 0

part2:: String -> Int
part2 = findBasement 0

main = do
  input <- readFile "in.txt"
  print . part1 $ input
  print . part2 $ input

