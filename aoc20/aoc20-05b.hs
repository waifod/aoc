import           Data.List
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

mySeatID :: [Int] -> Int
mySeatID ns = f $ sort ns
    where f (n1:n2:ns)
              | n1 + 2 == n2 = n1 + 1
              | otherwise    = f (n2:ns)

solve :: String -> Int
solve = mySeatID . map seatID . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
