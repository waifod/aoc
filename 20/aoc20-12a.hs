import           System.Environment

type Pos = (Int, Int)

data Dir = F | R | L | N | S | E | W deriving (Eq, Read)

type Ins = (Dir, Int)

getIns :: String -> Ins
getIns str = (read $ take 1 str, read $ tail str)

rotate :: Dir -> Ins -> Dir
rotate d (d', n) = dirs !! ((j + i * (n `quot` 90)) `mod` 4)
    where dirs = [E,N,W,S]
          i = if d' == L then 1 else -1
          j = length $ takeWhile (/= d) dirs

move :: (Pos, Dir) -> Ins -> (Pos, Dir)
move ((x,y),d) (d',n) = case d' of
                           F -> case d of
                                    N -> ((x, y + n), d)
                                    S -> ((x, y - n), d)
                                    E -> ((x + n, y), d)
                                    W -> ((x - n, y), d)
                           N -> ((x, y + n), d)
                           S -> ((x, y - n), d)
                           E -> ((x + n, y), d)
                           W -> ((x - n, y), d)
                           _ -> ((x, y), rotate d (d', n))

distance :: Pos -> Int
distance (x,y) = abs x + abs y

solve :: String -> Int
solve = distance . fst . foldl move ((0, 0), E) . map getIns . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
