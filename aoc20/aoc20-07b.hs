import           Data.Char
import           Data.String
import           Prelude
import           System.Environment
import           System.IO

data Tree a = Node a [Tree a]
type Rule = (String, [(String, Int)])

getRule :: String -> Rule
getRule str = (outBag, inBags)
    where getWords = map (filter isAlphaNum) . words
          outBag = concatWords . take 2 . getWords $ str
          inBags = f . drop 4 . getWords $ str
          f [] = []
          f (str:strs) | str == "no" = f (drop 3 strs)
                       | otherwise   = (concatWords $ take 2 strs, read str) : f (drop 3 strs)

concatWords :: [String] -> String
concatWords []         = ""
concatWords (str:strs) | not (null strs) = str ++ ' ' : concatWords strs
                       | otherwise       = str

contains :: [Rule] -> String -> [(String, Int)]
contains rules bag = snd . head . filter (\r -> fst r == bag) $ rules

treeFromNBags :: String -> Int -> [Rule] -> Tree Int
treeFromNBags bag n rules = Node n (map (\(bag', m) -> treeFromNBags bag' m rules)
                                   $ contains rules bag)

countBags :: Tree Int -> Int
countBags (Node n subtrees) = n + n * sum (map countBags subtrees)

solve :: String -> Int
solve = subtract 1 . countBags . treeFromNBags "shiny gold" 1 . map getRule . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
