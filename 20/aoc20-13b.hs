{-# LANGUAGE OverloadedStrings #-}
import           Data.List
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           System.Environment

parse :: T.Text -> [String]
parse = map T.unpack . T.splitOn "," . (!! 1) . T.lines

enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0..length xs] xs

iterateFind :: (a -> Bool) -> (a -> a) -> a -> a
iterateFind p f = go
    where go x
              | p x       = x
              | otherwise = go (f x)

minT :: [(Int, Int)] -> Int
minT = fst . foldl1' go
    where go (base, step) (offset, i) = (base', lcm step i)
              where base' = iterateFind (\n -> (n + offset) `rem` i == 0) (+ step) base

solve :: T.Text -> Int
solve = minT . map (fmap read) . filter (\(_, y) -> y /= "x") . enumerate . parse

main :: IO ()
main = do args <- getArgs
          content <- T.readFile (args !! 0)
          print $ solve content
