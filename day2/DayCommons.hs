module DayCommons (parseInputFile) where
import qualified Data.Text.Read as TR
import qualified Data.Text as T

parseInputFile :: String -> IO [[Integer]]
parseInputFile fp = do
    content <- readFile "input.txt"
    let ls = map T.pack (lines content)
    return (map parseLine ls)

mapNumber d = case d of
    Right (v, _) -> v

parseLine str = map (mapNumber . TR.decimal) (T.splitOn (T.pack " ") str)