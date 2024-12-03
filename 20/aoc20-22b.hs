{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Set           as S
import qualified Data.Text          as T
import           System.Environment

winner :: [Int] -> [Int] -> S.Set ([Int], [Int]) -> (Int, [Int])
winner [] ys _ = (2, ys)
winner xs [] _ = (1, xs)
winner xs@(x:xt) ys@(y:yt) history
    | S.member (xs, ys) history        = (1, xs)
    | x <= length xt && y <= length yt = case winnerSubgame of
                                             1 -> winner (xt ++ [x, y]) yt newHistory
                                             _ -> winner xt (yt ++ [y, x]) newHistory
    | x < y                            = winner xt (yt ++ [y, x]) newHistory
    | otherwise                        = winner (xt ++ [x, y]) yt newHistory
    where newHistory    = S.insert (xs, ys) history
          winnerSubgame = fst $ winner (take x xt) (take y yt) S.empty

score :: [Int] -> Int
score xs = sum $ zipWith (*) [1..length xs] $ reverse xs

solve :: String -> Int
solve str = score $ snd $ winner deck1 deck2 S.empty
    where [deck1, deck2] = map deck $ T.splitOn "\n\n" $ T.pack str
          deck = map (read . T.unpack) . tail . T.lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
