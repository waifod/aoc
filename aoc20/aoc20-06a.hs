import           Data.List
import           Data.String
import           Prelude
import           System.Environment
import           System.IO

groupAnswers :: [String] -> [String] -> [[String]]
groupAnswers grouped (str:strs) | null strs = [str : grouped]
                                | null str  = grouped : groupAnswers [] strs
                                | otherwise = groupAnswers (str : grouped) strs

countGroupYes :: [String] -> Int
countGroupYes = length . nub . concat

solve :: String -> Int
solve = sum . map countGroupYes . groupAnswers [] . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
