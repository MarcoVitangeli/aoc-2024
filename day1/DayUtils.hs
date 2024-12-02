module DayUtils (parseLines) where
import qualified Data.Text as T 
import qualified Data.Text.Read as TR (decimal)

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
