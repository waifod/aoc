import           System.Environment

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil p (x:xs)
    | p x       = xs
    | otherwise = dropUntil p xs

getIds :: String -> [Int]
getIds [] = []
getIds str = map read $ f [] str
    where f [] [] = []
          f gpd [] = [reverse gpd]
          f gpd (c:cs)
              | c == 'x'              = f [] cs
              | null gpd && c == ','  = f [] cs
              | gpd /= [] && c == ',' = (reverse gpd) : f [] cs
              | otherwise             = f (c : gpd) cs

earliest :: Int -> Int -> Int
earliest arr idBus = (1 + quot (arr - 1) idBus) * idBus

takeBusAt :: Int -> [Int] -> (Int, Int)
takeBusAt arr ids = (\n -> (n, f n)) $ foldl1 (\n m -> if f n < f m then n else m) ids
    where f = earliest arr

solve :: String -> Int
solve str = (\(idBus, t) -> idBus * (t - arr)) $ takeBusAt arr ids
    where arr = read $ takeWhile (/= '\n') str
          ids = getIds $ dropUntil (== '\n') str

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
