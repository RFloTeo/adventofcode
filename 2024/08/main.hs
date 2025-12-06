{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move filter" #-}
import System.IO
import qualified Data.Map as Map
import Data.List

type Vector = (Int, Int)
type Rect = (Vector, Vector)

addVector (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
subVector (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
mulVector factor (x,y) = (factor * x, factor * y)

insideRect:: Rect -> Vector -> Bool
insideRect ((x, y), (x', y')) (vx, vy) =
  vx >= min x x' && vx <= max x x' && vy >= min y y' && vy <= max y y'


parseMap:: [String] -> Map.Map Char [Vector]
parseMap ls =
    Map.fromListWith (++) . filter (\(k,v) -> k /= '.') . concat $  keysWithCoords
  where
    keysWithX = map (`zip` [0..]) ls
    addY:: Int -> (Char, Int) -> (Char, [Vector])
    addY y (key, x) = (key, [(x, y)])
    keysWithCoords = zipWith (\list y -> map (addY y) list) keysWithX [0..]

anti:: Vector -> (Vector, Vector) -> [Vector]
anti limits (v, v') = filter (insideRect ((0, 0), limits)) [a1, a2]
  where
    a1 = subVector (mulVector 2 v) v'
    a2 = subVector (mulVector 2 v') v

cmmdc:: (Int, Int) -> Int
cmmdc (a, b)
  | x `mod` y == 0 = y
  | otherwise = cmmdc (y, x - y)
  where
    x = max (abs a) (abs b)
    y = min (abs a) (abs b)

jumpWhile:: (Vector -> Bool) -> Vector -> Vector -> [Vector]
jumpWhile f a b
  | f b = b : jumpWhile f b c
  | otherwise = []
  where
    c = subVector (mulVector 2 b) a

anti':: Vector -> (Vector, Vector) -> [Vector]
anti' limits (v, v') = jumpWhile inBounds v v1
  where
    dif@(x,y) = subVector v' v
    cmd = cmmdc dif
    dif' = (x `div` cmd, y `div` cmd)
    v1 = addVector v dif'
    inBounds = insideRect ((0,0), limits)

getAntinodes:: (Vector -> (Vector, Vector) -> [Vector]) ->  Vector -> [Vector] -> [Vector]
getAntinodes antiF limits antennae =
    concatMap (antiF limits) [(x, y) | x <- antennae, y <- antennae, x /= y]

doPart:: (Vector -> (Vector, Vector) -> [Vector]) -> [String] -> Int
doPart antiF s = length . nub . concatMap (getAntinodes antiF limits) . Map.elems . parseMap $ s
  where
    limits = subVector (length . head $ s, length s) (1, 1)

part1:: [String] -> Int
part1 = doPart anti

part2:: [String] -> Int
part2 = doPart anti'

main = do
  input <- readFile "in.txt"
  let ls = lines input
  print . part1 $ ls
  print . part2 $ ls
