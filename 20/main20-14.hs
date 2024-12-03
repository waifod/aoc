{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import qualified Data.Array           as A
import           Data.Attoparsec.Text
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Prelude              hiding (takeWhile)
import           System.Environment

wordSize :: Int
wordSize = 36


data Bit = One | Zero | Floating
    deriving (Show, Eq)

type Memory = M.Map (Int) Int

--data Instruction = Mask (A.Array Int Bit) | Write Int Int
--    deriving (Show)
data Instruction = Mask [(Int, Bit)] | Write Int Int
    deriving (Show)

type Program = [Instruction]

-- No more ghetto parsing

parseMask :: Parser Instruction
parseMask = do
    string "mask = "
    k <- takeWhile (/= '\n')
    pure . Mask . conv . enumerate . reverse . T.unpack $ k
    where conv ((a, 'X'):xs) = (a, Floating):conv xs
          conv ((a, '1'):xs) = (a, One)     : conv xs
          conv ((a, '0'):xs) = (a, Zero)    : conv xs
          conv _             = []

parseMem :: Parser Instruction
parseMem = do
    string "mem["
    k <- decimal
    string "] = "
    r <- decimal
    return $ Write k r

parseInput :: Parser Program
parseInput = many ((parseMask <|> parseMem) <* many endOfLine)

stdarr :: [a] -> A.Array Int a
stdarr k = A.listArray (0, size - 1) k
    where size = length k

enumerate k = zip [0..length k] k

int2bits :: Int -> [(Int, Bit)]
int2bits k = enumerate . (++ replicate len Zero) . go $ k
    where go 0 = []
          go 1 = [One]
          go n | n `mod` 2 == 1 = One : go ( n `div` 2)
               | otherwise      = Zero: go ( n `div` 2)
          len = wordSize - (length $ go k)



bits2int :: [(Int, Bit)] -> Int
bits2int = foldl (+) 0 . fmap (\(x, y) -> conv y * 2^x)
    where conv Zero = 0
          conv One  = 1


--apply :: [(Int, Bit)] -> [(Int, Bit)] -> [(Int, Bit)]
apply m xs = foldl (\z (x, y) -> M.insert x y z) m xs


execute :: Program -> [(Int, Bit)] -> Memory -> Memory
execute [] _ m = m
execute ((Mask ks):xs) mask m = execute xs ks m --Overwrite mask
execute ((Write addr num):xs) mask m = execute xs mask m'
    where m'= M.insert addr (bits2int (M.assocs r')) m
          r'= (apply r (floatFilter mask)) -- Apply Mask
          r = apply M.empty bin -- Write number
          bin = int2bits num

floatFilter = filter (\(_, k) -> k /= Floating)

execute2 :: Program -> [(Int, Bit)] -> Memory -> Memory
execute2 [] _ m = m
execute2 ((Mask ks):xs) mask m = execute2 xs ks m --Overwrite mask
execute2 ((Write addr num):xs) mask m = execute2 xs mask m'
    where m'= apply m changes
          r'= M.elems (apply r (zeroFilter mask)) -- Apply Mask
          r = apply M.empty bin -- Take adress
          bin = int2bits addr
          changes = [(bits2int x, num) | x <- (enumerate <$> branches r') ]


branches [] = [[]]
branches (Zero:xs) = (Zero:) <$> (branches xs)
branches (One:xs) = (One:) <$> (branches xs)
branches (Floating:xs) = concat [(One:) <$> branches xs, (Zero:) <$> branches xs]


zeroFilter = filter (\(_, k) -> k /= Zero)



main = do
  args <- getArgs
  content <- T.readFile (args !! 0)
  case parseOnly parseInput content of
    Right r -> do
        putStrLn $ "1:" ++ (show . sum . M.elems $ execute r [] (M.empty))
        putStrLn $ "2:" ++ (show . sum . M.elems $ execute2 r [] (M.empty))
    Left _  -> print "lmao"
