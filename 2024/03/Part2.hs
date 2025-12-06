import System.IO
import Data.Char

parse :: Bool -> String -> [(Int, Int)]
parse _ "" = []
parse False ('d':'o':'(':')':s) = parse True s
parse True ('d':'o':'n':'\'':'t':'(':')':s) = parse False s
parse True ('m':'u':'l':'(':s) = parse' s 0 0
parse enabled (c:cs) = parse enabled cs

parse' :: String -> Int -> Int -> [(Int, Int)]
parse' (c:cs) x first
  | x >= 1000 = parse True (c:cs)
  | c == ',' = getSecond
  | c == ')' = getEnd
  | isDigit c = parse' cs newX first
  | otherwise = parse True cs
  where
    getSecond = if first > 0 then parse True cs else parse' cs 0 x
    getEnd = if first > 0 then (first, x) : parse True cs else parse True cs
    newX = x * 10 + digitToInt c

calc :: [(Int, Int)] -> Int
calc = foldr ((+) . mul) 0
  where
    mul (a, b) = a * b

textToResult = show . calc . parse True

main :: IO ()
main = do
  contents <- readFile "in.txt"
  putStrLn (textToResult contents)

