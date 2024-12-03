{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text          as T
import           System.Environment

winner :: Ord a => [a] -> [a] -> (Int, [a])
winner [] ys = (2, ys)
winner xs [] = (1, xs)
winner (x:xs) (y:ys)
    | x < y     = winner xs (ys ++ [y, x])
    | otherwise = winner (xs ++ [x, y]) ys

score :: [Int] -> Int
score xs = sum $ zipWith (*) [1..length xs] $ reverse xs

solve :: String -> Int
solve str = score $ snd $ winner deck1 deck2
    where [deck1, deck2] = map deck $ T.splitOn "\n\n" $ T.pack str
          deck = map (read . T.unpack) . tail . T.lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
