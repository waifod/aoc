{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Text            as T
import           System.Environment
import           System.IO

data Expr = Add Expr Expr | Mul Expr Expr | Val Int deriving (Read, Show)

eval :: Expr -> Int
eval = \case
    Val k     -> k
    Add e1 e2 -> eval e1 + eval e2
    Mul e1 e2 -> eval e1 * eval e2

encode :: Parser Expr
encode = undefined

parseSum :: Parser Expr
parseSum = do k <- decimal
              string " + "
              expr <- parseExpr
              return $ Add (Val k) expr

parseProd :: Parser Expr
parseProd = do k <- decimal
               string " * "
               expr <- parseExpr
               return $ Mul (Val k) expr

parsePar :: Parser Expr
parsePar = do char ')'
              exprPar <- parseExpr
              char '('
              c1 <- peekChar
              if c1 == Just ' '
                 then do c2 <- anyChar
                         expr <- parseExpr
                         case c2 of
                             '+' -> return $ Add exprPar expr
                             '*' -> return $ Mul exprPar expr
                 else return exprPar

parseExpr :: Parser Expr
parseExpr = parsePar <|> parseSum <|> parseProd <|> (Val <$> decimal)

main :: IO ()
main = undefined
