import System.IO

adder = 10000000000000

getMinCoef:: Integer -> Integer -> Integer -> Integer
getMinCoef a b s =
    head . dropWhile (\x -> (a * x) `mod` b /= mb) $ [0..]
  where
    mb = s `mod` b

cmmdc:: (Integer, Integer) -> Integer
cmmdc (a, b)
  | x `mod` y == 0 = y
  | otherwise = cmmdc (y, x - y)
  where
    x = max (abs a) (abs b)
    y = min (abs a) (abs b)

cmmmc:: (Integer, Integer) -> Integer
cmmmc (a,b) = a * b `div` cmmdc (a,b)

getScore':: [Integer] -> Integer
getScore' [ax, ay, bx, by, px, py]
  | actualPx `mod` cx /= 0 || actualPy `mod` cy /= 0 = 0
  | otherwise = calcMinPrice [nax, nbx, npx, mtx, nay, nby, npy, mty]
  where
    actualPx = adder + px
    actualPy = adder + py
    cx = cmmdc (ax, bx)
    cy = cmmdc (ay, by)
    [nax, nbx, npx] = map (`div` cx) [ax, bx, actualPx]
    [nay, nby, npy] = map (`div` cy) [ay, by, actualPy]
    mtx = getMinCoef nax nbx npx
    mty = getMinCoef nay nby npy

calcMinPrice:: [Integer] -> Integer
calcMinPrice [ax, bx, px, ix, ay, by, py, iy]
  | (nx, ny) == (-1, -1) = 0
  | diff `mod` diffMod /= 0 = 0
  | (jx < jy && ax * by > ay * bx) || (jx > jy && ax * by < ay * bx) = 0
  | njx == njy = 3 * (ix+ nx * bx + iAdd) + njx
  | otherwise = 0
  where
    (nx, ny) = getMinBAdds [ix, bx, iy, by]
    aix = ax * (ix + nx * bx)
    aiy = ay * (iy + ny * by)
    jx = (px - aix) `div` bx
    jy = (py - aiy) `div` by
    diff = abs (jx - jy)
    cx = cmmdc (ax,bx)
    cy = cmmdc (ay,by)
    cb = cmmmc (bx `div` cx, by `div` cy)
    diffMod = abs (ax * cb `div` bx - ay * cb `div` by)
    diffDiv = diff `div` diffMod
    iAdd = diffDiv * cb
    njx = (px - aix - ax * iAdd) `div` bx
    njy = (py - aiy - ay * iAdd) `div` by

-- min nx,ny so that a + i * x = b + j * y
getMinBAdds:: [Integer] -> (Integer, Integer)
getMinBAdds [a, i, b, j]
  | null result = (-1, -1)
  | otherwise = ((b - a + j * y) `div` i, y)
  where
    result = dropWhile (\y -> (b - a + j * y) `mod` i /= 0) [0..i]
    y = head result

getScore:: [Integer] -> Integer
getScore l
  | null result = 0
  | otherwise = head result
  where
    result = dropWhile (== 0) . map (calcPriceWithB l) $ [100,99..0]

calcPriceWithB:: [Integer] -> Integer -> Integer
calcPriceWithB [ax, ay, bx, by, px, py] b
  | restX < 0 || restY < 0 = 0
  | restX `mod` ax == 0 && restY `mod` ay == 0 && qx == qy && qx <= 100 = 3 * qx + b
  | otherwise = 0
  where
    restX = px - b * bx
    restY = py - b * by
    qx = restX `div` ax
    qy = restY `div` ay


part1:: [[Integer]] -> Integer
part1 = sum . map getScore

part2:: [[Integer]] -> Integer
part2 = sum . map getScore'

main = do
  input <- readFile "in.txt"
  let ls = map (map read . words) .lines $ input
  print . part1 $ ls
  print . part2 $ ls
