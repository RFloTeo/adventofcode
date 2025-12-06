import System.IO


-- SPLIT SECTION START --
splitOn:: Char -> String -> [String]
splitOn sep s = splitOn' sep s ""

splitOn':: Char -> String -> String -> [String]
splitOn' _ [] acc = [acc]
splitOn' sep (c:cs) acc
  | sep == c = acc : splitOn' sep cs ""
  | otherwise = splitOn' sep cs (acc ++ [c])
-- SPLIT SECTION END --

parseLine:: String -> [Int]
parseLine = map read . splitOn 'x'

getArea:: [Int] -> Int
getArea [x,y,z] =
    2 * s + 2 * s' + 2 * s'' + slack
  where
    s = x * y
    s' = y * z
    s'' = x * z
    slack = minimum [s,s',s'']

getRibbon:: [Int] -> Int
getRibbon ds@[x,y,z] =
  2 * (sum ds - maximum ds) + product ds

part1:: [String] -> Int
part1 = sum . map (getArea . parseLine)

part2:: [String] -> Int
part2 = sum . map (getRibbon . parseLine)

main = do
  input <- readFile "in.txt"
  let ls = lines input
  print . part1 $ ls
  print . part2 $ ls
