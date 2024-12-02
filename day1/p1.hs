import qualified Data.Text as T 
import qualified Data.List as DL (sort)

import qualified DayUtils as DU (parseLines)

computeDistance :: [Integer] -> [Integer] -> Integer -> Integer
computeDistance (x:xs) (y:ys) acc = computeDistance xs ys (acc + abs (x-y)) 
computeDistance [] [] acc = acc

main = do
  content <- readFile "input.txt"
  let ls = lines content
  let lss = map T.pack ls
  let (ll, rr) = DU.parseLines lss [] []
  print (computeDistance (DL.sort ll) (DL.sort rr) 0)
