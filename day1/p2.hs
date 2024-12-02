import qualified Data.Text as T 

import qualified DayUtils as DU (parseLines)

findOcurr :: [Integer] -> Integer -> Integer -> Integer
findOcurr (x:xs) p counter | x == p = findOcurr xs p (counter+1)
                           | otherwise = findOcurr xs p counter
findOcurr [] x counter = counter * x

similarity :: [Integer] -> [Integer] -> Integer -> Integer
similarity (x:xs) searchList acc = similarity xs searchList (acc + findOcurr searchList x 0)
similarity [] _ acc = acc

main = do
  content <- readFile "input.txt"
  let ls = lines content
  let lss = map T.pack ls
  let (ll, rr) = DU.parseLines lss [] []
  print $ similarity ll rr 0
