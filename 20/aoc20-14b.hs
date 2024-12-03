import           Data.List
import           Data.String
import           System.Environment

len :: Int
len = 36

type Bits = [Int]

type Addr = Int

type Mask = String

int2bits :: Int -> Bits
int2bits 0 = []
int2bits n = n `rem` 2 : int2bits (n `quot` 2)

bits2int :: Bits -> Int
bits2int []       = 0
bits2int (b:bits) = b + 2 * bits2int bits

makeLen :: Bits -> Bits
makeLen bits = take len (bits ++ repeat 0)

parse :: [String] -> [(Mask, Bits, Int)]
parse [] = []
parse (str:strs) = (map (\(a,b) -> (getMask str, a, b)) mems) ++ parse rem
    where mems = map getIns $ takeWhile (isPrefixOf "mem") strs
          rem = dropWhile (isPrefixOf "mem") strs

getIns :: String -> (Bits, Int)
getIns str = ( makeLen $ int2bits $ read $ drop 4 $ takeWhile (/= ']') str
             , read $ drop 2 $ dropWhile (/= '=') str
             )

getMask :: String -> Mask
getMask str = reverse $ drop 7 str

mask' :: Mask -> Bits -> [Bits]
mask' _ [] = [[]]
mask' (c:cs) (b:bits)
    | c == '0' = map (b :) $ mask' cs bits
    | c == '1' = map (1 :) $ mask' cs bits
    | c == 'X' = map (0 :) (mask' cs bits) ++ map (1 :) (mask' cs bits)

mask :: (Mask, Bits, Int) -> [(Bits, Int)]
mask (str, bits, n) = map (\bits -> (bits, n)) $ mask' str bits

filterIns :: [(Bits, Int)] -> [Int]
filterIns = f []
    where f _ [] = []
          f xs ((bits,n):ts)
              | elem bits xs = f xs ts
              | otherwise    = n : f (bits : xs) ts

solve :: String -> Int
solve = sum . filterIns . reverse . concat . map mask . parse . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
