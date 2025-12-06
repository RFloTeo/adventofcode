import System.IO

-- TREE SECTION START --
type TreeContent = (Int, [Int])
data Tree where
  Leaf :: TreeContent -> Tree
  Node :: TreeContent -> Tree -> Tree -> Tree
  EmptyLeaf :: Tree
  deriving (Show)

addToTree:: Tree -> (Int, Int) -> Tree
addToTree EmptyLeaf (key, value) = Leaf (key, [value])
addToTree (Leaf (leafKey, vs)) (newKey, v)
  | newKey < leafKey = Node (leafKey, vs) (Leaf (newKey, [v])) EmptyLeaf
  | newKey > leafKey = Node (leafKey, vs) EmptyLeaf (Leaf (newKey, [v]))
  | otherwise = Leaf (leafKey, v:vs)
addToTree (Node (nodeKey, vs) child1 child2) (newKey, v)
  | newKey < nodeKey = Node (nodeKey, vs) (addToTree child1 (newKey, v)) child2
  | newKey > nodeKey = Node (nodeKey, vs) child1 (addToTree child2 (newKey, v))
  | otherwise = Node (nodeKey, v:vs) child1 child2

searchTree:: Tree -> (Int, Int) -> Bool
searchTree EmptyLeaf _ = False
searchTree (Leaf (leafKey, vs)) (key, value) = leafKey == key && value `elem` vs
searchTree (Node (nodeKey, vs) child1 child2) s@(key, value)
  | key < nodeKey = searchTree child1 s
  | key > nodeKey = searchTree child2 s
  | otherwise = value `elem` vs
-- TREE SECTION END -- 

-- SPLIT SECTION START --
splitOn:: Char -> String -> [String]
splitOn sep s = splitOn' sep s ""

splitOn':: Char -> String -> String -> [String]
splitOn' _ [] acc = [acc]
splitOn' sep (c:cs) acc
  | sep == c = acc : splitOn' sep cs ""
  | otherwise = splitOn' sep cs (acc ++ [c])
-- SPLIT SECTION END --

-- PARSING SECTION START --
buildRules:: [String] -> Tree
buildRules rawRules =
    foldl addToTree EmptyLeaf (map parseRule rawRules)
  where
    --sstoIt: strings to Int tuple--
    ssToIt:: [String] -> (Int, Int)
    ssToIt [s1, s2] = (read s1, read s2)
    ssToIt _ = error "Wrong input detected while building rules"
    parseRule = ssToIt . splitOn '|'

buildUpdates:: [String] -> [[Int]]
buildUpdates = map (map read . splitOn ',')

splitInput:: String -> ([String], [String])
splitInput input =
    f ls []
  where
    f (x:xs) acc = if null x then (acc, xs) else f xs (x:acc)
    f [] acc = (acc, []) -- shouldn't happen
    ls = lines input

processInput:: String -> (Tree, [[Int]])
processInput input =
    (buildRules rules, buildUpdates updates)
  where
    (rules, updates) = splitInput input
-- PARSING SECTION END --

validUpdate:: Tree -> [Int] -> Bool
validUpdate rules update =
    (not . any (searchTree rules)) pairs
  where
    l = length update - 1
    pairs = [(update !! y,update !! x)| x <- [0..l], y <- [x+1..l]]

getMiddle:: [Int] -> Int
getMiddle xs =
  let mid = (length xs - 1) `div` 2 in xs !! mid

part1:: String -> Int
part1 input =
    (sum . map getMiddle . filter (validUpdate rules)) updates
  where
    (rules, updates) = processInput input

fixUpdate:: Tree -> [Int] -> [Int]
fixUpdate _ [] = []
fixUpdate rules (n:ns) =
    fixUpdate rules left ++ [n] ++ fixUpdate rules right
  where
    left = [x | x <- ns, searchTree rules (x, n)]
    right = [x | x <- ns, not (searchTree rules (x, n))]

part2:: String -> Int
part2 input =
    (sum . map (getMiddle . fixUpdate rules) . filter (not . validUpdate rules)) updates
  where
    (rules, updates) = processInput input

main = do
  input <- readFile "in.txt"
  print (part1 input)
  print (part2 input)
