import           Data.Char
import           System.Environment
import           Text.ParserCombinators.ReadP

num :: ReadP Int
num = read <$> many1 (satisfy isDigit)

prec :: ReadP Int
prec = ((*) <$> add <*> (string " * " *> prec)) +++ add
    where brack = (between (char '(') (char ')') prec) +++ num
          add = ((+) <$> brack <*> (string " + " *> add)) +++ brack

solve :: String -> Int
solve = sum . fst . head . parse
    where parse = readP_to_S ((sepBy prec (char '\n')) <* skipSpaces <* eof)

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
