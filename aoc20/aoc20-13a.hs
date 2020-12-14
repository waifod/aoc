{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           System.Environment

parse :: T.Text -> (String, [String])
parse txt = (arr, ids)
    where arr = T.unpack $ head $ T.lines txt
          ids = filter notX $ map T.unpack $ T.splitOn "," $ T.lines txt !! 1
          notX = \str -> str /= "x"

wait :: Int -> Int -> Int
wait arr idBus = idBus - rem arr idBus

takeBusWait :: Int -> [Int] -> (Int, Int)
takeBusWait arr = (\n -> (n, f n)) . foldl1 g
    where f = wait arr
          g = \n m -> if f n < f m then n else m

solve :: T.Text -> Int
solve txt = (\(idBus, t) -> idBus * t) $ takeBusWait arr ids
    where arr = read $ fst $ parse txt
          ids = map read $ snd $ parse txt

main :: IO ()
main = do args <- getArgs
          content <- T.readFile (args !! 0)
          print $ solve content
