import           Data.List
import           Data.String
import           Prelude
import           System.Environment
import           System.IO

groupAnswers :: [String] -> [String] -> [[String]]
groupAnswers grouped (str:strs) | null strs = [str : grouped]
                                | null str  = grouped : groupAnswers [] strs
                                | otherwise = groupAnswers (str : grouped) strs

allYes :: Char -> [String] -> Bool
allYes c = and . map (elem c)

countAllYes :: [String] -> Int
countAllYes strs = length $ filter (\f -> f strs) checkYes
    where checkYes = [allYes c | c <- ['a'..'z']]

solve :: String -> Int
solve = sum . map countAllYes . groupAnswers [] . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
