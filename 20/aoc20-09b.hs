import           System.Environment

len :: Int
len = 25

valid :: [Int] -> [Int]
valid []     = []
valid (n:ns) = map (+ n) ns ++ valid ns

newBatch :: Int -> [Int] -> [Int]
newBatch n ns = n : init ns

firstNotValid :: [Int] -> Int
firstNotValid ints = g batch ns
    where batch  = reverse $ take len ints
          ns     = drop len ints
          g batch (n:ns)
              | n `elem` valid batch = g (newBatch n batch) ns
              | otherwise            = n

contSublist :: [Int] -> [Int]
contSublist ns = g [] ns
    where k = firstNotValid ns
          g cnt ints@(n:ns)
              | sum cnt + n < k = g (n : cnt) ns
              | n == k          = g [] ns
              | sum cnt + n > k = g (init cnt) ints
              | otherwise       = n : cnt

solve :: String -> Int
solve = diff . contSublist . map read . lines
    where diff = \ns -> maximum ns + minimum ns

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
