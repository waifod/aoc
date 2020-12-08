import           Data.Char
import           Data.String
import           Prelude
import           System.Environment
import           System.IO

data Op = Acc Char Int | Jmp Char Int | NOp Char Int

storeIns :: [String] -> [Op]
storeIns [] = []
storeIns (ins:inss) = case take 3 ins of
                          "acc" -> Acc sign num : storeIns inss
                          "jmp" -> Jmp sign num : storeIns inss
                          "nop" -> NOp sign num : storeIns inss
    where sign = head $ drop 4 ins
          num  = read $ drop 5 ins

process :: Int -> Int -> [Int] -> [Op] -> Int
process acc n done inss | n `elem` done = acc
                        | otherwise     = case inss !! n of
                                              Acc '+' k -> process (acc + k) (n + 1) (n : done) inss
                                              Acc '-' k -> process (acc - k) (n + 1) (n : done) inss
                                              Jmp '+' k -> process acc (n + k) (n : done) inss
                                              Jmp '-' k -> process acc (n - k) (n : done) inss
                                              NOp c   k -> process acc (n + 1) (n : done) inss

solve :: String -> Int
solve = process 0 0 [] . storeIns . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
