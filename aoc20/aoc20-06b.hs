import qualified Data.Set           as S
import           Data.String
import           System.Environment
import           System.IO

grpAns :: [S.Set Char] -> [[S.Set Char]]
grpAns [] = []
grpAns (ans:anss)
    | null ans = grpAns anss
    | otherwise = f [ans] anss
    where f gpd [] = [gpd]
          f gpd (ans:anss)
              | null ans  = gpd : grpAns anss
              | otherwise = f (ans : gpd) anss

solve :: String -> Int
solve = sum . map (S.size . foldl1 S.intersection) . grpAns . map S.fromList . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
