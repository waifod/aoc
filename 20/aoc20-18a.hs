import           Data.Char
import           System.Environment
import           Text.ParserCombinators.ReadP

num :: ReadP Int
num = read <$> many1 (satisfy isDigit)

l2r :: ReadP Int
l2r = foldl (flip ($)) <$> brack <*> (many (($) <$> op <*> brack))
    where op = ((+) <$ string " + ") +++ ((*) <$ string " * ")
          brack = (between (char '(') (char ')') l2r) +++ num

solve :: String -> Int
solve = sum . fst . head . parse
    where parse = readP_to_S ((sepBy l2r (char '\n')) <* skipSpaces <* eof)

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
