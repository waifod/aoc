{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text          as T
import           System.Environment

len :: Int
len = 2020

nextNum :: [Int] -> [Int]
nextNum ls@(n:ns) = f 0 n ns : ls
    where f k m [] = 0
          f k m (n:ns)
              | m == n    = k + 1
              | otherwise = f (k + 1) m ns

applyN :: Integral b => (a -> a) -> b -> a -> a
applyN f 0 = id
applyN f n = f . applyN f (n-1)

solve :: String -> Int
solve str = head $ applyN nextNum (len - length ls) ls
    where ls = reverse . map (read . T.unpack) $ T.splitOn "," $ T.pack str

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
