import           Data.Char
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

process :: Int -> Int -> [Op] -> Int
process acc n ops | n == length ops = acc
                  | otherwise        = case ops !! n of
                                           Acc k -> process (acc + k) (n + 1) ops
                                           Jmp k -> process acc (n + k) ops
                                           NOp k -> process acc (n + 1) ops

terminates :: Int -> [Int] -> [Op] -> Bool
terminates n done ops | n == length ops = True
                      | n `elem` done   = False
                      | otherwise       = case ops !! n of
                                              Jmp k -> terminates (n + k) (n : done) ops
                                              _     -> terminates (n + 1) (n : done) ops

altOps :: [Op] -> [[Op]]
altOps [] = []
altOps (op:ops) = case op of
                      Jmp num -> (NOp num : ops) : map (op :) (altOps ops)
                      NOp num -> (Jmp num : ops) : map (op :) (altOps ops)
                      _       -> map (op :) (altOps ops)

solve :: String -> Int
solve = process 0 0 . head . filter (terminates 0 []) . altOps . storeOps . words

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
