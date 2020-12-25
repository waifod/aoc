import qualified Data.Set           as S
import           System.Environment

type Pos = (Int, Int)

type Grid = S.Set Pos

neighbs :: Pos -> S.Set Pos
neighbs (x, y) = S.fromList [ (x + 1, y)
                            , (x + 1, y - 1)
                            , (x, y - 1)
                            , (x - 1, y)
                            , (x - 1, y + 1)
                            , (x, y + 1)
                            ]

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

newGrid :: Grid -> Grid
newGrid grid = S.filter (\p -> beBlack p grid) . S.unions . S.insert grid $ S.map neighbs grid
    where beBlack p grid
              | S.member p grid = blackNeighbs p > 0 && blackNeighbs p < 3
              | otherwise       = blackNeighbs p == 2
          blackNeighbs p = S.size $ S.intersection (neighbs p) grid

solve :: String -> Int
solve = S.size . (!! 100) . iterate newGrid . parse

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    print $ solve content
