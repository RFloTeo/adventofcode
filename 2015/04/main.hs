import System.IO
import Data.Hash.MD5
import Data.List
import Data.Maybe

hashWithNum:: String -> Int -> String
hashWithNum s n = md5s . Str $ (s ++ show n)

doPart:: String -> String -> Int
doPart prefix s = fromJust . find (isPrefixOf prefix . hashWithNum s) $ [1..]

main = do
  let s = "ckczppom"
  print . doPart "00000" $ s
  print . doPart "000000" $ s
