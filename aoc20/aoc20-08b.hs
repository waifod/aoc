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

process :: Int -> Int -> [Op] -> Int
process acc n inss | n == length inss = acc
                   | otherwise        = case inss !! n of
                                            Acc '+' k -> process (acc + k) (n + 1) inss
                                            Acc '-' k -> process (acc - k) (n + 1) inss
                                            Jmp '+' k -> process acc (n + k) inss
                                            Jmp '-' k -> process acc (n - k) inss
                                            NOp c   k -> process acc (n + 1) inss

terminates :: Int -> [Int] -> [Op] -> Bool
terminates n done inss | n == length inss = True
                       | n `elem` done    = False
                       | otherwise        = case inss !! n of
                                                Jmp '+' k -> terminates (n + k) (n : done) inss
                                                Jmp '-' k -> terminates (n - k) (n : done) inss
                                                _         -> terminates (n + 1) (n : done) inss

altOps :: [Op] -> [[Op]]
altOps [] = []
altOps (ins:inss) = case ins of
                        Jmp sign num -> (NOp sign num : inss) : map (ins :) (altOps inss)
                        NOp sign num -> (Jmp sign num : inss) : map (ins :) (altOps inss)
                        _            -> map (ins :) (altOps inss)

solve :: String -> Int
solve = process 0 0 . head . filter (terminates 0 []) . altOps . storeIns . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
