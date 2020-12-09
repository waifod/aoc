import qualified Data.Set           as S
import           Data.String
import           System.Environment
import           System.IO

grpAns :: S.Set Char -> [S.Set Char] -> [S.Set Char]
grpAns gpd (ans:anss)
    | null anss  = [S.union gpd ans]
    | S.null ans = gpd : grpAns S.empty anss
    | otherwise  = grpAns (S.union gpd ans) anss

solve :: String -> Int
solve = sum . map S.size . grpAns S.empty . map S.fromList . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
