import           Data.List
import           Data.String
import           System.Environment

type Bits = [Int]

type Addr = Int

type Mask = String

int2bits :: Int -> Bits
int2bits 0 = []
int2bits n = n `rem` 2 : int2bits (n `quot` 2)

bits2int :: Bits -> Int
bits2int []       = 0
bits2int (b:bits) = b + 2 * bits2int bits

make36 :: Bits -> Bits
make36 bits = take 36 (bits ++ repeat 0)

parse :: [String] -> [(Mask, Addr, Bits)]
parse [] = []
parse (str:strs) = (map (\(a,b) -> (getMask str, a, b)) mems) ++ parse rem
    where mems = map getIns $ takeWhile (isPrefixOf "mem") strs
          rem = dropWhile (isPrefixOf "mem") strs

getIns :: String -> (Addr, Bits)
getIns str = ( read $ drop 4 $ takeWhile (/= ']') str
              , make36 $ int2bits $ read $ drop 2 $ dropWhile (/= '=') str
              )

getMask :: String -> Mask
getMask str = reverse $ drop 7 str

mask' :: Mask -> Bits -> Bits
mask' _ [] = []
mask' (c:cs) (b:bits)
    | c == 'X' = b : mask' cs bits
    | c == '0' = 0 : mask' cs bits
    | c == '1' = 1 : mask' cs bits

mask :: (Mask, Addr, Bits) -> Bits
mask (str, _, bits) = reverse $ mask' str bits

filterMem :: [(Mask, Addr, Bits)] -> [(Mask, Addr, Bits)]
filterMem = f []
    where f _ [] = []
          f xs (t@(_,mem,_):ts)
              | elem mem xs = f xs ts
              | otherwise   = t : f (mem : xs) ts

solve :: String -> Int
solve = sum . map (bits2int . reverse) . map mask . filterMem . reverse . parse . lines

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
