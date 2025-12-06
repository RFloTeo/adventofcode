import System.IO

searchList :: [String] -> [((Int, Int), (Int, Int), (Int, Int))]
searchList ss =
		[((x, y), d1, d2) | 
			x <- [1..rows-2],
			y <- [1..cols-2],
			d1 <- [(1, 1), ((-1), (-1))],
			d2 <- [(1, (-1)), ((-1), 1)]
		]
	where
		rows = length ss
		cols = (length . head) ss 
		
getLetter :: [String] -> (Int, Int) -> Char
getLetter text (x, y) = (text !! x) !! y

addVector (x1, y1) (x2, y2) = (x1+x2, y1+y2)
subVector (x1, y1) (x2, y2) = (x1-x2, y1-y2)

search :: [String] -> ((Int, Int), (Int, Int), (Int, Int)) -> Int
search text (curr, d1, d2)
	| centr && diag && diag' = 1
	| otherwise = 0
	where
		get = getLetter text
		centr = get curr == 'A'
		m = addVector curr d1
		s = subVector curr d1
		m' = addVector curr d2
		s' = subVector curr d2
		diag = (get m == 'M') && (get s == 'S') 
		diag' = (get m' == 'M') && (get s' == 'S')

textToResult :: String -> String
textToResult s =
		(show . sum . map (search text) . searchList) text
	where
		text = lines s

main = do
  text <- readFile "in.txt"
  putStrLn (textToResult text) 
