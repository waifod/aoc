import           Data.List
import           System.Environment

result :: [Int] -> Int
result ns = len1 * (len3 + 1)
    where [len1, len3] = map length . group . sort $ zipWith (-) (tail ns) ns

solve :: String -> Int
solve = result . (0 :) . sort . map read . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
