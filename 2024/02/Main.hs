module Main where
import System.IO

intXor :: Int -> Int -> Int
intXor a b =
  a + b - (a * b)

dampList :: [Int] -> Int -> [Int]
dampList l prev
  | prev == (-1) = l
  | otherwise = prev:l

isDirSafe :: [Int] -> Int -> Int -> Int -> Int
isDirSafe (x1:x2:xs) direction damping prev
  | damping >= 2 = 0
  | distance > 3 = damp
  | distance == 0 = damp
  | followingDirection = isDirSafe (x2 : xs) direction damping x1
  | otherwise = damp
  where
    distance = abs (x1 - x2)
    followingDirection = signum (x2 - x1) == direction
    damp2List = dampList (x2:xs) prev
    damp1 = isDirSafe (x1:xs) direction (damping + 1) prev
    damp2 = isDirSafe damp2List direction (damping + 1) prev
    damp = intXor damp1 damp2
isDirSafe (x1:[]) _ damping _
  | damping < 2 = 1
  | otherwise = 0
isDirSafe [] _ damping _
  | damping < 2 = 1
  | otherwise = 0

isSafe :: Int -> [Int] -> Int
isSafe initialDamp xs =
    intXor upSafe downSafe
  where
    upSafe = isDirSafe xs 1 initialDamp (-1)
    downSafe = isDirSafe xs (-1) initialDamp (-1)

safeLists :: [[Int]] -> Int
safeLists ls =
    foldr (+) 0 processed
  where
    processed = map (isSafe 0) ls -- pass 1 to isSafe for part 1 and 0 for part 2

stringToInt :: String -> Int
stringToInt s = read s

splitTextToNums :: String -> [[Int]]
splitTextToNums t =
  map (map stringToInt) (map words (lines t))

textToResult = show . safeLists . splitTextToNums

main :: IO ()
main = do
  contents <- readFile "in.txt"
  putStrLn (textToResult contents)
