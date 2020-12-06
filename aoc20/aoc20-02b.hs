import           Data.Char
import           Data.String
import           Prelude
import           System.Environment
import           System.IO

xor :: Bool -> Bool -> Bool
xor True  = not
xor False = id

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil p (x:xs) | p x       = xs
                   | otherwise = dropUntil p xs

splitStr :: String -> [String]
splitStr [] = []
splitStr xs = takeWhile isAlphaNum xs : splitStr remainder
    where remainder = dropWhile (not . isAlphaNum) $ dropUntil (not . isAlphaNum) xs

rearrange :: [String] -> (Int,Int,Char,String)
rearrange (x1:x2:x3:x4:_) = (read x1, read x2, head x3, x4)

isValid :: (Int,Int,Char,String) -> Bool
isValid (x1,x2,x3,x4) = ((x4 !! (x1 - 1)) == x3) `xor` ((x4 !! (x2 - 1)) == x3)

solve :: String -> Int
solve = length . filter (isValid . rearrange . splitStr) . lines

main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
