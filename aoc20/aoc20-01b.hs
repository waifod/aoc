import           Data.String
import           Prelude
import           System.Environment
import           System.IO

nuples :: Int -> [a] -> [[a]]
nuples 0 _      = [[]]
nuples _ []     = []
nuples n (x:xs) = map (x:) (nuples (n-1) xs) ++ nuples n xs

isValid :: [Int] -> Bool
isValid ns = sum ns == 2020

solve :: String -> Int
solve = product . head . filter isValid . nuples 3 . map read . lines

main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
