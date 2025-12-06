import System.IO
import Data.List

hasNVowels:: Int -> String -> Bool
hasNVowels x [] = x <= 0
hasNVowels x (c:cs)
  | x <= 0 = True
  | c `elem` "aeiou" = hasNVowels (x - 1) cs
  | otherwise = hasNVowels x cs

doubleLetter:: String -> Bool
doubleLetter "" = False
doubleLetter [c] = False
doubleLetter (c:c':cs)
  | c == c' = True
  | otherwise = doubleLetter (c':cs)

noBadString:: [String] -> String -> Bool
noBadString bads s = not . any (`isInfixOf` s) $ bads

isNice:: String -> Bool
isNice s = hasNVowels 3 s && doubleLetter s && noBadString ["ab", "cd", "pq", "xy"] s

-- no consecutive duplicates: removes one instance where there are 2 consecutive duplicates --
nocd:: Eq a => [a] -> [a]
nocd [] = []
nocd [x] = [x]
nocd (x:x':xs)
  | x == x' = x : nocd xs
  | otherwise = x : nocd (x':xs)

hasRepeatingPair:: String -> Bool
hasRepeatingPair s = (length . nub $ ss) < length ss
  where
    ss = nocd . map (take 2) . tails $ s

hasAbaPattern:: String -> Bool
hasAbaPattern s = any (\s -> length s > 2 && head s == (s !! 2)) (tails s)

isNice':: String -> Bool
isNice' s = hasRepeatingPair s && hasAbaPattern s

part1:: [String] -> Int
part1 = length . filter isNice

part2:: [String] -> Int
part2 = length . filter isNice'

main = do
  input <- readFile "in.txt"
  let ls = lines input
  print . part1 $ ls
  print . part2 $ ls
