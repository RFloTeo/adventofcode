import System.IO

searchList :: [String] -> [(Int, Int, Int, Int)]
searchList ss =
		[(x, y, dx, dy) |
			x <- [0..rows-1],
			y <- [0..cols-1],
			dx <- [-1..1],
			dy <- [-1..1],
			abs dx + abs dy > 0
		]
	where
		rows = length ss
		cols = (length . head) ss 

letter :: Int -> Char
letter 0 = 'X'
letter 1 = 'M'
letter 2 = 'A'
letter 3 = 'S'
letter n = ' '

search :: [String] -> Int -> (Int, Int, Int, Int) -> Int
search text progress (x, y, dx, dy) 
	| progress == 4 = 1
	| outOfBounds = 0
	| curr == letter progress = search text (progress + 1) (x+dx, y+dy, dx, dy)
	| otherwise = 0
	where
		rows = length text
		cols = length (text !! x) 
		outOfBounds = x < 0 || x >= rows || y < 0 || y >= cols
		curr = (text !! x) !! y

textToResult :: String -> String
textToResult s =
		(show . sum . map (search text 0) . searchList) text
	where
		text = lines s

main = do
  text <- readFile "in.txt"
  putStrLn (textToResult text) 
