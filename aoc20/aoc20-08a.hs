import           Data.Char
import           Data.List
import           Data.String
import           Prelude
import           System.Environment
import           System.IO

data Op = Acc Int | Jmp Int | NOp Int

storeOps :: [String] -> [Op]
storeOps [] = []
storeOps (ins:n:strs) = case ins of
                            "acc" -> Acc num : storeOps strs
                            "jmp" -> Jmp num : storeOps strs
                            "nop" -> NOp num : storeOps strs
    where num  = read $ if head n == '+' then tail n else n

process :: Int -> Int -> [Int] -> [Op] -> Int
process acc n done ops | n `elem` done = acc
                       | otherwise     = case ops !! n of
                                             Acc k -> process (acc + k) (n + 1) (n : done) ops
                                             Jmp k -> process acc (n + k) (n : done) ops
                                             NOp k -> process acc (n + 1) (n : done) ops

solve :: String -> Int
solve = process 0 0 [] . storeOps . words

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
