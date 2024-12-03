import           System.Environment

subjCard :: Int
subjCard = 7

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = takeWhile (not . p)

nextVal :: Int -> Int -> Int
nextVal subj val = (val * subj) `rem` 20201227

genLoop :: Int -> [Int]
genLoop subj = iterate (nextVal subj) 1

loopSize :: Int -> Int -> Int
loopSize pubKey = length . takeUntil (== pubKey) . genLoop

solve :: String -> Int
solve str = genLoop pubKey2 !! size1
    where
        [pubKey1, pubKey2] = map read . lines $ str
        size1              = loopSize pubKey1 subjCard

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    print $ solve content
