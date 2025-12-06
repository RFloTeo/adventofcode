module Main where
import System.IO
import Data.Char

parse :: String -> [(Int, Int)]
parse "" = []
parse ('m':'u':'l':'(':s) = parse' s 0 0
parse (c:cs) = parse cs

parse' :: String -> Int -> Int -> [(Int, Int)]
parse' (c:cs) x first
  | x >= 1000 = parse (c:cs)
  | c == ',' = getSecond
  | c == ')' = getEnd
  | isDigit c = parse' cs newX first
  | otherwise = parse cs
  where
    getSecond = if first > 0 then parse cs else parse' cs 0 x
    getEnd = if first > 0 then (first, x) : parse cs else parse cs
    newX = x * 10 + digitToInt c

calc :: [(Int, Int)] -> Int
calc = foldr ((+) . mul) 0
  where
    mul (a, b) = a * b

textToResult = show . calc . parse

main :: IO ()
main = do
  contents <- readFile "in.txt"
  putStrLn (textToResult contents)
