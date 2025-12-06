import System.IO
import Data.Char
import Data.Maybe

buildLists:: String -> Int -> Int -> ([(Int, Int)], [Int])
buildLists "" _ _ = ([], [])
buildLists (c:cs) k id =
    (newElems ++ files, spaces)
  where
    n = digitToInt c
    newElems = zip (replicate n id) [k..k + n - 1]
    (files, spaces) = buildLists' cs (k + n) (id + 1)

buildLists'::String -> Int -> Int -> ([(Int, Int)], [Int])
buildLists' "" _ _ = ([], [])
buildLists' (c:cs) k id =
    (files, [k..k + n - 1] ++ spaces)
  where
    n = digitToInt c
    (files, spaces) = buildLists cs (k + n) id

defrag:: [(Int, Int)] -> [Int] -> [(Int, Int)]
defrag [] _ = []
defrag ((k, v):fs) (s:ss)
  | s >= v = (k,v):fs
  | otherwise = (k, s) : defrag fs ss

checksum:: [(Int, Int)] -> Int
checksum = sum . map (uncurry (*))

part1:: String -> Int
part1 s =
    checksum $ defrag revFiles spaces
  where
    (files, spaces) = buildLists s 0 0
    revFiles = reverse files

type File = (Int, Int, Int) -- id, start, size
type Space = (Int, Int) -- start, size

buildBlocks:: String -> Int -> Int -> ([File], [Space])
buildBlocks "" _ _ = ([], [])
buildBlocks (c:cs) k id = 
    ((id, k, n) : files, spaces)
  where
    n = digitToInt c
    (files, spaces) = buildBlocks' cs (k + n) (id + 1)

buildBlocks':: String -> Int -> Int -> ([File], [Space])
buildBlocks' "" _ _ = ([], [])
buildBlocks' (c:cs) k id =
    (files, (k, n) : spaces)
  where
    n = digitToInt c
    (files, spaces) = buildBlocks cs (k + n) id

defrag':: [File] -> [Space] -> [File]
defrag' [] _ = []
defrag' (f@(id, start, size) : fs) spaces
  | start <= (fst . head $ spaces) = f : fs
  | isJust spStart = (id, fromJust spStart, size) : defrag' fs newSpaces
  | otherwise = f : defrag' fs spaces
  where
    (spStart, newSpaces) = findOpenSpace spaces start size

findOpenSpace:: [Space] -> Int -> Int-> (Maybe Int, [Space])
findOpenSpace [] _ _ = (Nothing, [])
findOpenSpace (s@(start, spSize):spaces) reqStart reqSize
  | reqStart <= start = (Nothing, s:spaces)
  | reqSize > spSize = (result, s:newSpaces)
  | reqSize == spSize = (Just start, spaces)
  | otherwise = (Just start, (start + reqSize, spSize - reqSize) : spaces)
  where
    (result, newSpaces) = findOpenSpace spaces reqStart reqSize

checksum':: [File] -> Int
checksum' = sum . map (\(id, start, size) -> id * (start * size + (size * (size - 1) `div` 2)))

part2:: String -> Int
part2 s = checksum' $ defrag' revFiles spaces
  where
    (files, spaces) = buildBlocks s 0 0
    revFiles = reverse files

main = do
  input <- readFile "in.txt"
  let ls = take 19999 input
  print . part1 $ ls
  print . part2 $ ls
