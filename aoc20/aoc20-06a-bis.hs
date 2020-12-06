{-# LANGUAGE OverloadedStrings #-}
import           Data.List          (foldl', foldl1, sort)
import qualified Data.Set           as S
import qualified Data.Text          as T
import           System.Environment

answers x = map people $ groups
  where
    groups = T.splitOn "\n\n" x
    people = T.splitOn "\n"


main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let p = T.pack content
  let answerlist = fmap (fmap T.unpack) (answers p)
  let answersets = fmap (fmap S.fromList) answerlist
  print $ sum $ fmap (S.size . (foldl1 S.union)) $ answersets
