import           System.Environment

type Pos = (Int, Int)

data Dir = F | R | L | N | S | E | W deriving (Eq, Read)

type Ins = (Dir, Int)

getIns :: String -> Ins
getIns str = (read $ take 1 str, read $ tail str)

rotate :: Pos -> Int -> Pos
rotate (x, y) l = case l of
                      90  -> (-y, x)
                      180 -> (-x, -y)
                      270 -> (y, -x)

move :: (Pos, Pos) -> Ins -> (Pos, Pos)
move ((x,y),(x',y')) (d,l) = case d of
                                 F -> ((x + l * x', y + l * y'), (x',y'))
                                 R -> ((x,y), rotate (x',y') (360-l))
                                 L -> ((x,y), rotate (x',y') l)
                                 N -> ((x,y), (x', y' + l))
                                 S -> ((x,y), (x', y' - l))
                                 E -> ((x,y), (x' + l, y'))
                                 _ -> ((x,y), (x' - l, y'))

distance :: Pos -> Int
distance ((x,y)) = abs x + abs y

solve :: String -> Int
solve = distance . fst . foldl move ((0, 0), (10, 1)) . map getIns . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
