import           System.Environment

type Pos = (Int, Int)

data Dir = F | R | L | N | S | E | W deriving (Eq, Read)

type Ins = (Dir, Int)

getIns :: String -> Ins
getIns str = (read $ take 1 str, read $ tail str)

rotate :: Dir -> Ins -> Dir
rotate d (d', l) = dirs !! ((j + i * (l `quot` 90)) `mod` 4)
    where dirs = [E,N,W,S]
          i = if d' == L then 1 else -1
          j = length $ takeWhile (/= d) dirs

move :: (Pos, Dir) -> Ins -> (Pos, Dir)
move ((x,y),d) (d',l) = case d' of
                           F -> case d of
                                    N -> ((x, y + l), d)
                                    S -> ((x, y - l), d)
                                    E -> ((x + l, y), d)
                                    W -> ((x - l, y), d)
                           R -> ((x, y), rotate d (R, l))
                           L -> ((x, y), rotate d (L, l))
                           N -> ((x, y + l), d)
                           S -> ((x, y - l), d)
                           E -> ((x + l, y), d)
                           _ -> ((x - l, y), d)

distance :: Pos -> Int
distance (x,y) = abs x + abs y

solve :: String -> Int
solve = distance . fst . foldl move ((0, 0), E) . map getIns . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
