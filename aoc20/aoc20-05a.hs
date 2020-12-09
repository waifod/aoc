import           Data.String
import           System.Environment
import           System.IO

str2int :: String -> Int
str2int xs = f 0 xs
    where f n [] = n
          f n (x:xs)
              | x == 'B' || x == 'R' = f (2 * n + 1) xs
              | otherwise            = f (2 * n) xs

seatID :: String -> Int
seatID str = 8 * row + col
    where row = str2int $ take 7 str
          col = str2int $ drop 7 str

solve :: String -> Int
solve = maximum . map seatID . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
