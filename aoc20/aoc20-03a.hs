import           Data.String
import           Prelude
import           System.Environment
import           System.IO

path :: Int -> Int -> Int -> [String] -> String
path h v p xs | length xs <= v = []
              | otherwise      = (xs !! v !! newP) : path h v newP (drop v xs)
    where len  = length $ head xs
          newP = (p + h) `rem` len

nTrees :: String -> Int
nTrees = length . filter (=='#')

solve :: String -> Int
solve str = product $ map (\f -> nTrees $ f 0 grid) slopes
    where grid  = lines str
          slopes = [path 3 1]

main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
