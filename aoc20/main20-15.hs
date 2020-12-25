{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import qualified Data.IntMap.Strict as M
import           Data.List
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           System.Environment

data S = S {mem   :: !(M.IntMap Int),
            iter  :: Int,
            lastn :: Int}
    deriving (Show)

game xs p = loop getnext (S m offset k) (p-offset-1)
    where start = init xs
          k = last xs
          m = startingmem xs
          offset = length xs
          loop f !s 0  = (f s)
          loop f !s !k = loop f (f s) (k-1)


getnext !S {..} = S newmem (iter+1) age where
    newmem = M.insert lastn iter mem
    age = case M.lookup lastn mem of
        Nothing -> 0
        Just a  -> iter - a

startingmem xs = M.fromList $ zip xs [1..]

main = do
  args <- getArgs
  content <- T.readFile (args !! 0)
  let k = T.splitOn "," content
  let parsed = fmap (read . T.unpack) k
  putStrLn $ "1:" ++ (show . lastn $ game parsed 2020)
  putStrLn $ "2:" ++ (show . lastn $ game parsed 30000000)
