import           Data.String
import           System.Environment
import           System.IO

len :: Int
len = 3

year :: Int
year = 2020

nuples :: Int -> [a] -> [[a]]
nuples 0 _      = [[]]
nuples _ []     = []
nuples n (x:xs) = map (x:) (nuples (n-1) xs) ++ nuples n xs

isValid :: [Int] -> Bool
isValid ns = sum ns == year

solve :: String -> Int
solve = product . head . filter isValid . nuples len . map read . lines

main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
