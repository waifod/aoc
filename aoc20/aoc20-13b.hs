import           Data.List
import           System.Environment

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil p (x:xs)
    | p x       = xs
    | otherwise = dropUntil p xs

getIdsPaired :: String -> [(Int, Int)]
getIdsPaired [] = []
getIdsPaired str = f 0 [] str
    where f _ [] [] = []
          f k gpd [] = [(k, read $ reverse gpd)]
          f k gpd (c:cs)
              | c == 'x'              = f (k + 1) [] cs
              | null gpd && c == ','  = f k [] cs
              | gpd /= [] && c == ',' = (k, read $ reverse gpd) : f (k + 1) [] cs
              | otherwise             = f k (c : gpd) cs

iterateFind :: (a -> Bool) -> (a -> a) -> a -> a
iterateFind p f = go
    where go x
              | p x       = x
              | otherwise = go (f x)

minT :: [(Int, Int)] -> Int
minT = fst . foldl1' go
    where go (base, step) (offset, i) = (base', step * i)
              where base' = iterateFind (\n -> (n + offset) `mod` i == 0) (+ step) base

solve :: String -> Int
solve = minT . getIdsPaired . dropUntil (== '\n')

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
