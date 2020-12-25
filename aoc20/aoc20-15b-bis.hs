{-# LANGUAGE OverloadedStrings #-}
import           Data.List          (elemIndex)
import           Data.Maybe         (maybe)
import qualified Data.Text          as T
import           System.Environment

vanEck :: Int -> [Int] -> [Int]
vanEck n ns = reverse $ iterate go ns !! n
    where go xxs@(x:xs) = maybe 0 succ (elemIndex x xs) : xxs

len :: Int
len = 30000000

solve :: String -> Int
solve str = last $ vanEck (len - length ls) (reverse ls)
    where ls = map (read . T.unpack) $ T.splitOn "," $ T.pack str

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
