import           Data.Char
import qualified Data.Set           as S
import           Data.String
import           Prelude
import           System.Environment
import           System.IO

type Rule = (String, [(String, Int)])

getRule :: String -> Rule
getRule str = (outBag, inBags)
    where getWords = map (filter isAlphaNum) . words
          outBag = concatWords . take 2 . getWords $ str
          inBags = f . drop 4 . getWords $ str
          f [] = []
          f (str:strs) | str == "no" = f (drop 3 strs)
                       | otherwise   = (concatWords $ take 2 strs, read str) : f (drop 3 strs)

isDirContained :: String -> [Rule] -> S.Set String
isDirContained bag = S.fromList . map fst . filter (elem bag . map fst . snd)

isContained :: String -> [Rule] -> S.Set String
isContained bag rules = h (f bag) (f bag)
    where f = (\b -> isDirContained b rules)
          g = S.unions . S.map f
          h cont bags | (g bags) `S.isSubsetOf` cont = cont
                      | otherwise                    = h newCont newBags
              where newCont = S.union cont (g bags)
                    newBags = S.difference (g bags) cont

concatWords :: [String] -> String
concatWords []         = ""
concatWords (str:strs) | not (null strs) = str ++ ' ' : concatWords strs
                       | otherwise       = str

solve :: String -> Int
solve = length . isContained "shiny gold" . map getRule . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
