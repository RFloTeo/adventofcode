import System.IO

part1:: [String] -> Int
part1 s = 0

part2:: [String] -> Int
part2 s = 0

main = do
  input <- readFile "in.txt"
  let ls = lines input
  print . part1 $ ls
  print . part2 $ ls
