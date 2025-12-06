{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
import System.IO

data State = Moving Vector Direction [Vector] | Done [Vector] deriving (Show)
type Vector = (Int, Int)
data Direction = Up | Rt | Dn | Lt
  deriving (Eq, Show, Enum)

turnRight:: Direction -> Direction
turnRight Lt = Up
turnRight d = succ d

step:: Direction -> Vector
step Up = (0, -1)
step Rt = (1, 0)
step Dn = (0, 1)
step Lt = (-1, 0)

advance:: [String] -> State -> State
advance _ (Done l) = Done l
advance maze (Moving loc@(x,y) dir hist)
  | outOfBounds = Done hist
  | obstacleAhead = Moving loc (turnRight dir) hist
  | otherwise = Moving newLoc dir newHist
  where
    outOfBounds = x < 0 || y < 0 || x >= length (head maze) || y >= length maze
    newLoc = addVector loc (step dir)
    obstacleAhead = getLetter maze newLoc == '#'
    newHist = if loc `elem` hist then hist else loc:hist

isDone (Moving {}) = False
isDone (Done _) = True

getHistory (Moving _ _ h) = h
getHistory (Done h) = h

doUntil:: (a -> a) -> (a -> Bool) -> a -> a
doUntil f p x = if p x then x else doUntil f p (f x)

getLetter :: [String] -> Vector -> Char
getLetter text (x, y)
  | outOfBounds = ' '
  | otherwise = (text !! y) !! x
  where
    outOfBounds = x < 0 || y < 0 || x >= length (head text) || y >= length text

addVector (x1, y1) (x2, y2) = (x1+x2, y1+y2)
subVector (x1, y1) (x2, y2) = (x1-x2, y1-y2)
eqVector (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

indexOf:: (Eq a) => [a] -> a -> Int
indexOf [] _ = -1 --not found
indexOf (x:xs) s
  | x == s = 0
  | otherwise = 1 + indexOf xs s

getGuard:: [String] -> Vector
getGuard maze = (x,y)
  where
    rowSize = length . head $ maze
    colSize = length maze
    oneLiner = concat maze
    result = indexOf oneLiner '^'
    y = result `div` rowSize
    x = result `mod` rowSize

part1:: String -> Int
part1 = length . getPath

getPath:: String -> [Vector]
getPath input =
    getHistory result
  where
    maze = lines input
    takeStep = advance maze
    initialState = Moving (getGuard maze) Up []
    result = doUntil takeStep isDone initialState

addObstacle:: [String] -> Vector -> [String]
addObstacle maze (x,y) =
    take y maze ++ [editedRow] ++ drop (y+1) maze
  where
    originalRow = maze !! y
    editedRow = take x originalRow ++ "#" ++ drop (x + 1) originalRow

hasCycle:: [String] -> Vector -> Bool
hasCycle maze vector =
    not . isDone . fst $ doUntil (\(x, y) -> (advance maze' . advance maze' $ x, advance maze' y)) doneOrMet (hareState, tortoiseState)
  where
    maze' = addObstacle maze vector
    tortoiseState = Moving (getGuard maze') Up []
    hareState = advance maze' tortoiseState
    doneOrMet (Done _, _) = True
    doneOrMet (Moving v1 d1 _, Moving v2 d2 _) = eqVector v1 v2 && d1 == d2

part2:: String -> Int
part2 input =
    length . filter (hasCycle maze) $ originalPath
  where
    maze = lines input
    originalPath = getPath input

main = do
  input <- readFile "in.txt"
  print (part1 input)
  print (part2 input)
