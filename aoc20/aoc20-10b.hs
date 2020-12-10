import           Data.List
import           System.Environment

reducs :: [Int] -> Int
reducs [_] = 1
reducs [_,_] = 1
reducs (n1:n2:n3:ns)
    | n1 + 4 > n3 = product (map reducs $ split3 (n1:n3:ns)) + reducs (n2:n3:ns)
    | otherwise   = reducs (n2:n3:ns)

split3 :: [Int] -> [[Int]]
split3 = f []
    where f grp [] = [reverse grp]
          f grp (n:ns)
              | null grp         = f [n] ns
              | head grp + 3 > n = f (n : grp) ns
              | otherwise        = (reverse grp) : f [n] ns

solve :: String -> Int
solve = product . map reducs . split3 . (0 :) . sort . map read . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content