import           Data.Char
import qualified Data.Set           as S
import           Data.String
import           Prelude
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
          f (str:strs) | str == "no" = []
                       | otherwise   = (concat $ take 2 strs, read str) : f (drop 3 strs)

containDir :: Bag -> [Rule] -> S.Set Bag
containDir bag = S.fromList . map fst . filter (elem bag . map fst . snd)

contain :: Bag -> [Rule] -> S.Set Bag
contain bag rules = h (f bag) (f bag)
    where f = \b -> containDir b rules
          g = S.unions . S.map f
          h cont bags | S.null newBags = cont
                      | otherwise      = h newCont newBags
              where newCont = S.union cont (g bags)
                    newBags = S.difference (g bags) cont

solve :: String -> Int
solve = length . contain "shinygold" . map getRule . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
