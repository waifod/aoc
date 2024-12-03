{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import qualified Data.Array           as A
import           Data.Attoparsec.Text
import           Data.List
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           System.Environment

data Rule = C Char | LR [Int] | OR Rule Rule deriving Show

parseC = do i <- decimal
            string ": \""
            c <- letter
            char '\"'
            return (i, C c)

parseLR = do i <- decimal
             string ": "
             ns <- decimal `sepBy` (char ' ')
             return (i, LR ns)

parseOR = do i <- decimal
             string ": "
             ms <- decimal `sepBy` (char ' ')
             string " | "
             ns <- decimal `sepBy` (char ' ')
             return (i, OR (LR ms) (LR ns))

parser :: Parser [(Int, Rule)]
parser = many ((parseC <|> parseOR <|> parseLR) <* many endOfLine)

stdArray :: [a] -> A.Array Int a
stdArray xs = A.listArray (0, length xs - 1) xs

makeArray :: Ord a => [(a, b)] -> A.Array Int b
makeArray = stdArray . map snd . sortBy (\(m,_) (n,_) -> compare m n)

startC :: Char -> [String] -> [String]
startC c strs = [xs | (x:xs) <- strs, x == c]

multR :: [Int] -> A.Array Int Rule -> [String] -> [String]
multR [] rules     = id
multR (n:ns) rules = multR ns rules . applyR (rules A.! n) rules

applyR :: Rule -> A.Array Int Rule -> [String] -> [String]
applyR _ _     []   = []
applyR r rules strs = case r of
                          C c      -> startC c strs
                          LR ns    -> multR ns rules strs
                          OR r1 r2 -> applyR r1 rules strs ++ applyR r2 rules strs

isValid :: A.Array Int Rule -> String -> Bool
isValid rules = not . null . filter (== "") . applyR (rules A.! 0) rules . (: [])

solve :: T.Text -> [(Int, Rule)] -> Int
solve txt r = length $ filter (isValid rules) strs
    where strs  = map T.unpack $ T.lines txt
          rules = makeArray r

main :: IO ()
main = do args <- getArgs
          content <- T.readFile (args !! 0)
          let [rules, txt] = T.splitOn "\n\n" content
          case parseOnly parser rules of
              Right r -> print $ solve txt r
