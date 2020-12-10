import           Data.List
import           System.Environment

result :: [Int] -> Int
result = f 1 1
    where f c1 c3 (n1:ns)
              | null ns      = c1 * c3
              | n1 + 1 == n2 = f (c1 + 1) c3 ns
              | n1 + 2 == n2 = f c1 c3 ns
              | otherwise    = f c1 (c3 + 1) ns
              where n2 = head ns

solve :: String -> Int
solve = result . sort . map read . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
