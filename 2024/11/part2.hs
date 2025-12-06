import System.IO

doSteps:: Int -> Integer -> Int
doSteps x num
	| x <= 0 = 1
	| num == 0 = doSteps (x - 1) 1
	| even digits = doSteps (x - 1) frst + doSteps (x-1) scnd
	| otherwise = doSteps (x - 1) (num * 2024)
	where
		nStr = show num
		digits = length nStr
		half = digits `div` 2
		frst = read . take half $ nStr
		scnd = read . drop half $ nStr

main = do
	input <- readFile "in.txt"
	let inp = sum . map (doSteps 75 . read) . words . takeWhile (/= '\n') $ input
	print inp
