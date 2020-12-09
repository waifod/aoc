import           Data.String
import           Prelude
import           System.Environment
import           System.IO

valid :: [Int] -> [Int]
valid []     = []
valid (n:ns) = map (+ n) ns ++ valid ns

newBatch :: Int -> [Int] -> [Int]
newBatch n ns = n : init ns

firstNotValid :: [Int] -> Int
firstNotValid ints = g batch ns
    where batch  = reverse $ take 25 ints
          ns     = drop 25 ints
          g batch (n:ns) | n `elem` valid batch = g (newBatch n batch) ns
                         | otherwise            = n

solve :: String -> Int
solve = firstNotValid . map read . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
