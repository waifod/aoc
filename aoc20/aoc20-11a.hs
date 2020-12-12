import qualified Data.Array         as A
import           System.Environment

type Grid = A.Array Int (A.Array Int Char)

stdArray :: [a] -> A.Array Int a
stdArray xs = A.listArray (0, size - 1) xs
    where size = length xs

grid :: [String] -> Grid
grid = stdArray . map stdArray

takenAround :: Int -> Int -> Grid -> Int
takenAround m n grd = length [1 | (i,j) <- neighbs, (grd A.! i) A.! j == '#']
    where hb = snd $ A.bounds $ grd A.! 0
          vb = snd $ A.bounds grd
          neighbs = [(m+i,n+j) | i <- [-1..1], j <- [-1..1], (i,j) /= (0,0), m+i >= 0, m+i <= vb, n+j >= 0, n+j <= hb]

newStatus :: Int -> Int -> Grid -> Char
newStatus m n grd
    | c == 'L' && takenAround m n grd == 0 = '#'
    | c == '#' && takenAround m n grd > 3  = 'L'
    | otherwise                            = c
    where c = grd A.! m A.! n

newGrid :: Grid -> Grid
newGrid grd = f hb vb grd
    where hb = snd $ A.bounds $ grd A.! 0
          vb = snd $ A.bounds grd
          f m n grd = grid $ map (map (\(m',n') -> newStatus m' n' grd)) grid'
          grid' = map (\m -> map (\n -> (m,n)) [0..hb]) [0..vb]

finalGrid :: Grid -> Grid
finalGrid grd
    | newGrid grd == grd = grd
    | otherwise          = finalGrid $ newGrid grd

solve :: String -> Int
solve = length . filter (== '#') . concat . map A.elems . A.elems . finalGrid . grid . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
