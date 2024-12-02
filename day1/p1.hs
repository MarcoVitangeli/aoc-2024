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

computeDistance :: [Integer] -> [Integer] -> Integer -> Integer
computeDistance (x:xs) (y:ys) acc = computeDistance xs ys (acc + abs (x-y)) 
computeDistance [] [] acc = acc

main = do
  content <- readFile "input.txt"
  let ls = lines content
  let lss = map T.pack ls
  let (ll, rr) = parseLines lss [] []
  print (computeDistance (DL.sort ll) (DL.sort rr) 0)
