import           System.Environment

type Slope = Int -> [String] -> String

slopes :: [Slope]
slopes = [path 1 1, path 3 1, path 5 1, path 7 1, path 1 2]

path :: Int -> Int -> Slope
path h v p xs
    | length xs <= v = []
    | otherwise      = (xs !! v !! newP) : path h v newP (drop v xs)
    where len  = length $ head xs
          newP = (p + h) `rem` len

nTrees :: String -> Int
nTrees = length . filter (=='#')

solve :: String -> Int
solve str = product $ map (\f -> nTrees $ f 0 grid) slopes
    where grid = lines str

main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
