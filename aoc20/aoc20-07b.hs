import           Data.Char
import           Data.String
import           System.Environment
import           System.IO

type Bag = String
type Rule = (Bag, [(Bag, Int)])

getWords :: String -> [String]
getWords = map (filter isAlphaNum) . words

getRule :: String -> Rule
getRule str = (outBag, inBags)
    where outBag = concat . take 2 . getWords $ str
          inBags = f . drop 4 . getWords $ str
          f [] = []
          f (str:strs)
              | str == "no" = []
              | otherwise   = (concat $ take 2 strs, read str) : f (drop 3 strs)

containsDir :: Bag -> [Rule] -> [(Bag, Int)]
containsDir bag rules = snd . head . filter (\r -> fst r == bag) $ rules

countBags :: Int -> Bag -> [Rule] -> Int
countBags n bag rules = n + n * sum (map f $ containsDir bag rules)
    where f = \(bag',m) -> countBags m bag' rules

solve :: String -> Int
solve = subtract 1 . countBags 1 "shinygold" . map getRule . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
