{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Set           as S
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           System.Environment


type Memory = S.Set (Int, Int, Int)

--Not-so-ghetto parsing

parseInput x = zip [1..] . fmap parseLine $ T.lines x

parseLine x = zip [1..] (T.unpack x)

fillLine n ((k,'.'):xs) m = fillLine n xs m
fillLine n ((k,'#'):xs) m = fillLine n xs (S.insert (n, k, 0) m)
fillLine n [] m           = m

fillmem ((k, xs):ys) m = fillmem ys (fillLine k xs m)
fillmem [] m           = m

check mem (x, y, z) = length . filter id . fmap ((\x y -> S.member y x) mem ) $ neighbors (x, y, z)
check4 mem (x, y, z, w) = length . filter id . fmap ((\x y -> S.member y x) mem ) $ neighbors4 (x, y, z, w)

neighbors (x, y, z) = [(x+k1, y+k2, z+k3) | k1 <- [-1..1], k2 <-[-1..1], k3<-[-1..1], not $ k1 == 0 && k2 == 0 && k3 == 0]

neighbors4 (x, y, z, w) = [(x+k1, y+k2, z+k3, w+k4) |
    k1 <- [-1..1],
    k2 <-[-1..1],
    k3 <-[-1..1],
    k4 <-[-1..1],
    not $ k1 == 0 && k2 == 0 && k3 == 0 && k4 == 0]


updatem f c mem = S.union toKeep toAdd
    where active = mem
          frontier = S.unions . S.map (\x -> S.difference (S.fromList (f x)) active) $ active
          toKeep = S.filter live mem
          toAdd = S.filter born frontier
          live x = 2 == c mem x || 3 == c mem x
          born x = 3 == c mem x

applyn 0 f x = f x
applyn n f x = applyn (n-1) f (f x)

main = do
  args <- getArgs
  content <- T.readFile (args !! 0)
  let mem = fillmem (parseInput content) S.empty
  let mem4 = S.map (\(x, y, z) -> (x, y, z, 0)) mem
  putStrLn $ "1:" ++ (show (S.size . applyn 5 (updatem neighbors check) $ mem))
  putStrLn $ "2:" ++ (show (S.size . applyn 5 (updatem neighbors4 check4) $ mem4))
