import qualified Data.Text as T 
import qualified Data.Text.Read as TR (decimal)
import qualified Data.List as DL (sort)

parseLines :: [T.Text] -> [Integer] -> [Integer] -> ([Integer], [Integer])
parseLines [] left right = (left, right)
parseLines (x:xs) left right = let 
  sp = T.splitOn (T.pack " ") x
  f = filter (T.empty /=) sp in 
  case f of
  [x,y] -> 
    let
      Right (xVal, _) = TR.decimal x
      Right (yVal, _) = TR.decimal y
    in
      parseLines xs (xVal:left) (yVal:right)

findOcurr (x:xs) p counter | x == p = findOcurr xs p (counter+1)
                           | otherwise = findOcurr xs p counter
findOcurr [] x counter = counter * x

similarity (x:xs) searchList acc = similarity xs searchList (acc + findOcurr searchList x 0)
similarity [] _ acc = acc

main = do
  content <- readFile "input.txt"
  let ls = lines content
  let lss = map T.pack ls
  let (ll, rr) = parseLines lss [] []
  print $ similarity ll rr 0
