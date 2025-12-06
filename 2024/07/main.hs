import System.IO

type Line = (Integer, [Integer])
parseLine:: String -> Line
parseLine l =
    (test, params)
  where
    t:ts = words l
    test = read . takeWhile (/= ':') $ t
    params = map read ts

testLine:: Line -> Bool
testLine l = testWithOperators l []

applyOperators:: [Integer] -> [Integer -> Integer -> Integer] -> Integer
applyOperators ns ops =
    foldl applyOp (head ns) nsWithOps
  where
    applyOp x (y, f) = f x y
    nsWithOps = zip (tail ns) ops

testWithOperators:: Line -> [Integer -> Integer -> Integer] -> Bool
testWithOperators l@(test, params) ops
  | total > test = False
  | length ops < length params - 1 = testWithOperators l (ops ++ [(*)]) || testWithOperators l (ops ++ [(+)])
  | otherwise = total == test
  where
    total = applyOperators params ops

part1:: [String] -> Integer
part1 = sum . map fst . filter testLine . map parseLine

cc:: Integer -> Integer -> Integer
cc x y = read (show x ++ show y)

testLine':: Line -> Bool
testLine' l = testWithOperators' l []

testWithOperators':: Line -> [Integer -> Integer -> Integer] -> Bool
testWithOperators' l@(test, params) ops
  | total > test = False
  | length ops < length params - 1 = testWithOperators' l (ops ++ [cc]) || testWithOperators' l (ops ++ [(*)]) || testWithOperators' l (ops ++ [(+)])
  | otherwise = total == test
  where
    total = applyOperators params ops
  

part2:: [String] -> Integer
part2 = sum . map fst . filter testLine' . map parseLine

main = do
  input <- readFile "in.txt"
  let ls = lines input
  print . part1 $ ls
  print . part2 $ ls

