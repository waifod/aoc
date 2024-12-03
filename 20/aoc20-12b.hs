import           System.Environment

type Pos = (Int, Int)

data Dir = F | R | L | N | S | E | W deriving (Eq, Read)

type Ins = (Dir, Int)

getIns :: String -> Ins
getIns str = (read $ take 1 str, read $ tail str)

rotate :: Int -> Pos -> Pos
rotate 90 (x,y) = (-y,x)
rotate n (x,y)  = rotate (n - 90) (-y,x)

move :: (Pos, Pos) -> Ins -> (Pos, Pos)
move ((x,y),(x',y')) (d,n) = case d of
                                 F -> ((x + n * x', y + n * y'), (x',y'))
                                 N -> ((x,y), (x', y' + n))
                                 S -> ((x,y), (x', y' - n))
                                 E -> ((x,y), (x' + n, y'))
                                 W -> ((x,y), (x' - n, y'))
                                 R -> ((x,y), rotate (360-n) (x',y'))
                                 _ -> ((x,y), rotate n (x',y'))

distance :: Pos -> Int
distance ((x,y)) = abs x + abs y

solve :: String -> Int
solve = distance . fst . foldl move ((0, 0), (10, 1)) . map getIns . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
