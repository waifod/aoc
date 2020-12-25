import qualified Data.Set           as S
import           System.Environment

type Pos = (Int, Int)

type Grid = S.Set Pos

coord :: String -> Pos
coord ""         = (0, 0)
coord ('e':cs)   = let (x, y) = coord cs in (x+1, y)
coord ('w':cs)   = let (x, y) = coord cs in (x-1, y)
coord (c1:c2:cs) = let (x, y) = coord cs in case [c1, c2] of
                                                "se" -> (x+1, y-1)
                                                "sw" -> (x,   y-1)
                                                "ne" -> (x,   y+1)
                                                "nw" -> (x-1, y+1)

flipTile :: Pos -> Grid -> Grid
flipTile p grid
    | S.member p grid = S.delete p grid
    | otherwise       = S.insert p grid

parse :: String -> Grid
parse = foldr flipTile S.empty . map coord . lines

solve :: String -> Int
solve = S.size . parse

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    print $ solve content
