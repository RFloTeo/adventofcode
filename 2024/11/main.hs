import System.IO

doTimes:: Integer -> (a -> a) -> a -> a
doTimes x f v
  | x <= 0 = v
  | otherwise = doTimes (x - 1) f (f v)

step:: [Integer] -> [Integer]
step [] = []
step (x:xs)
  | x == 0 = 1 : step xs
  | even digits = firstHalf : secondHalf : step xs
  | otherwise = (x * 2024) : step xs
  where
    xStr = show x
    digits = length xStr
    half = digits `div` 2
    firstHalf = read (take half xStr)
    secondHalf = read (drop half xStr)

part1:: [Integer] -> Int
part1 = length . doTimes 25 step

part2:: [Integer] -> Int
part2 = length . doTimes 75 step

main = do
  input <- readFile "in.txt"
  let inp = map read . words . takeWhile (/= '\n') $ input
  print . part1 $ inp
  print . part2 $ inp
