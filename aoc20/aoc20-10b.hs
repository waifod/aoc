import           Data.List
import           System.Environment

split3 :: [Int] -> [[Int]]
split3 = f []
    where f grp [] = [reverse grp]
          f grp (n:ns)
              | null grp         = f [n] ns
              | head grp + 3 > n = f (n : grp) ns
              | otherwise        = (reverse grp) : f [n] ns

reducs :: [Int] -> Int
reducs (n1:n2:n3:ns)
    | n1 + 4 > n3 = reducs (n1:n3:ns) + reducs (n2:n3:ns) + 1
    | otherwise   = reducs (n2:n3:ns)
reducs _ = 0

solve :: String -> Int
solve = product . map reducs . split3 . (0 :) . sort . map read . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
