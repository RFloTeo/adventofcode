import System.IO

type Vector = (Int, Int)
addVector (x, y) (x', y') = (x + x', y + y')
mulVector c (x, y) = (c * x, c * y)
modVector (x, y) (x', y') = (x `mod` x', y `mod` y')

bounds = (101,103)

listToVectors [xp, yp, xv, yv] = ((xp,yp), (xv, yv))

takeEvery:: Int -> [a] -> [[a]]
takeEvery _ [] = []
takeEvery i xs = take i xs : takeEvery i (drop i xs)

simulateSeconds:: Int -> (Vector, Vector) -> Vector
simulateSeconds s (p, v) =
  modVector (addVector p (mulVector s v)) bounds

getQuadrant:: Vector -> Int
getQuadrant (x, y)
  | x < midX && y < midY = 1
  | x > midX && y < midY = 2
  | x < midX && y > midY = 3
  | x > midX && y > midY = 4
  | otherwise = 0
  where
    midX = fst bounds `div` 2
    midY = snd bounds `div` 2

part1:: [[Int]] -> Int
part1 i =
    product . map (length . (\q -> filter (== q) quads)) $ [1..4]
  where
    quads = map (getQuadrant . simulateSeconds 100 . listToVectors) i

getMap:: [Vector] -> String
getMap vs = unlines . takeEvery (fst bounds) . concatMap ((\s -> if s == "0" then " " else s) . show . length . (\v -> filter (== v) vs)) $ positions
  where
    positions = [(x, y) | y <- [0..(snd bounds - 1)], x <- [0..(fst bounds - 1)]]

part2:: [[Int]] -> Int -> IO ()
part2 i secs = do
  let mapStr = getMap . map (simulateSeconds secs . listToVectors) $ i
  print secs
  putStrLn (mapStr ++ "\n")
  _ <- getLine
  part2 i (secs + 1)

start2:: Int -> IO ()
start2 s = do
  input <- readFile "in.txt"
  let ls = map (map read . words) (lines input)
  part2 ls s

main = do
  input <- readFile "in.txt"
  let ls = map (map read . words) (lines input)
  print . part1 $ ls
