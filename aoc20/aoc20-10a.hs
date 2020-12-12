import           Data.List
import           System.Environment

result :: [Int] -> Int
result = f 0 1
    where f c1 c3 (n1:ns)
              | null ns           = c1 * c3
              | n1 + 1 == head ns = f (c1 + 1) c3 ns
              | otherwise         = f c1 (c3 + 1) ns

solve :: String -> Int
solve = result . (0 :) . sort . map read . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
